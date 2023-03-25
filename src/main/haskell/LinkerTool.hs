{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs 
import Data.Data
import Control.Monad
import LexML.URN.Types hiding (Data)
import LexML.URN.Parser
import LexML.Linker (refContextoFederal)
import LexML.Linker
import LexML.Linker(linker)
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
-- import qualified System.IO.UTF8 as IOUTF
import Control.Monad.Except
import Prelude hiding (log)
import LexML.Version
import qualified Data.ByteString as BIO
import Data.ByteString.UTF8 (toString, fromString)
import System.IO hiding (stdin,stdout)
import LexML.MD5
import GHC.IO.Handle.FD (stdin,stdout)

data Analise = Analise {
      frase :: String
    , entrada :: String
    , saida :: String
    , tipoEntrada :: TipoEntrada
    , tipoSaida :: TipoSaida
    , enderecoResolver :: String
    , contexto :: String
    , logaRegras :: Bool
    , logaTokens :: Bool
    , calculaMD5 :: Bool
    , debug :: Bool
  } deriving (Eq,Show,Read,Typeable,Data)

data TipoEntrada = TEXT | HXML deriving (Data,Typeable,Show,Eq,Read)

data TipoSaida = URNs | HTML | XML  deriving (Data,Typeable,Show,Eq,Read)

analise = Analise {
      frase = def &= typ "FRASE" &= help "Frase a ser analisada"
    , entrada = def &= typFile &= help "Arquivo de entrada. Se omitido ou especificado \"-\", então será usada a entrada padrão."
    , saida = def &= typFile &= help "Arquivo de saída. Se omitido ou especificado \"-\", então será usada a saída padrão."
    , tipoEntrada = enum [
            TEXT &= help "Texto de entrada sem mark-up (default)" 
          , HXML &= help "Texto de entrada com mark-up (XML/HTML)"
        ]   
    , tipoSaida = enum [
            URNs &= help "Gera lista de URNs reconhecidas (default)"
          , HTML &= help "Gera HTML decorado com links para o resolver do LexML"
          , XML  &= help "Gera XML decorado com as URNs reconhecidas"
        ]  
    , enderecoResolver = "https://www.lexml.gov.br/urn/URNLEXML" &= opt "https://www.lexml.gov.br/urn/URNLEXML" &= help "Endereço do LexML Resolver, a string URNLEXML será substituída pela URN. Usado quando com o tipo de saída HTML. Outro opção: 'https://normas.leg.br/?urn=URNLEXML'"
    , contexto = "federal" &= typ "URN" &= opt "federal" &= help "URN Lex ML de contexto. Podem ser usados os apelidos 'federal' e 'senado' para o contexto de leis federais e resoluções do senado, respectivamente, ou INLINE para ser lido alternadamente às linhas de entrada" 
    , logaRegras = False &= help "Loga a execução das regras"
    , logaTokens = False &= help "Loga a saída da análise léxica"
    , calculaMD5 = False &= help "Calcula o MD5 da entrada"
    , debug = False &= help "Mostra mensagens de debug"
  }

endMarker :: BIO.ByteString
endMarker = fromString "###LEXML-END###"

readUntilEndMarker handle = readUntilEndMarker' []
  where
    readUntilEndMarker' l = do
      eof <- hIsEOF handle
      if eof then return $ reverse l 
      else do
        line <- BIO.hGetLine handle
        if line == endMarker then return $ reverse l 
          else readUntilEndMarker' (line : l)          

readInput :: LinkerOptions -> Bool -> Maybe Handle -> (LinkerOptions -> String -> IO ()) -> IO ()
readInput lo ctxInline Nothing proc = readInput lo ctxInline (Just stdin) proc
readInput lo False (Just handle) proc = do
  cts <- BIO.hGetContents handle
  proc lo (toString cts)
readInput lo True (Just handle) proc = do
  eof <- hIsEOF handle
  if eof then return () else do 
    context <- hGetLine handle
    if context == "" then readInput lo True (Just handle) proc else do 
      ctx <- parseContexto context
      let lo' = lo { loContext = ctx }
      lines <- readUntilEndMarker handle
      proc lo' $ toString $ BIO.intercalate (fromString "\n") lines
      readInput lo True (Just handle) proc

      
parseContexto c@('u':'r':'n':':':_) = do
  mu <- parseURNLexML c
  case mu of
    Nothing -> return $ RC_Nome "federal"
    Just u  -> return $ RC_URNLexML u
parseContexto c = return $ RC_Nome c

main = do
  args <- cmdArgs analise
  let logLevel = if debug args then DEBUG else ERROR
  updateGlobalLogger rootLoggerName (setLevel logLevel)
  let ctxInline = contexto args == "INLINE" 
  contexto <- case contexto args of
                "INLINE" -> return $ RC_Nome "federal"
                "" -> return $ RC_Nome "federal"
                c@('u':'r':'n':':':_) -> do
                    mu <- parseURNLexML c
                    case mu of
                      Nothing -> return $ RC_Nome "federal"
                      Just u  -> return $ RC_URNLexML u
                c -> return $ RC_Nome c
  let lo = defaultLinkerOptions {
            loContext = contexto
          , loResolverUrl = case enderecoResolver args of
                              "" -> Nothing
                              er -> Just er
          , loOutputType = case tipoSaida args of
                URNs -> OT_URNS
                HTML -> OT_AHREF
                XML  -> OT_XLINK
          , loInputType = case tipoEntrada args of
                TEXT -> IT_TEXT
                HXML -> IT_TAGGED
          , loDebugRegras = logaRegras args
          , loDebugTokens = logaTokens args
          , loConstituicaoSimples = refContextoFederal contexto 
        }        
  let inputProc :: (LinkerOptions -> String -> IO ()) -> IO ()
      inputProc = case frase args of
        ""  -> (case entrada args of
          "" -> readInput lo ctxInline Nothing 
          "-" -> readInput lo ctxInline Nothing
          fname -> \proc -> (
            do handle <- openFile fname ReadMode
               readInput lo ctxInline (Just handle) proc
               hClose handle 
            )
          )
        txt -> \proc -> proc lo txt
       
  outHandle <- case saida args of
        "" -> return $ stdout
        "-" -> return $ stdout
        fname -> openFile fname WriteMode
  hSetEncoding outHandle utf8
  inputProc $ \lo input -> do
    res <- runExceptT $ linker lo input
    case res of
      Left err -> return ()
      Right out -> do
        let content = resContent out
        hPutStr outHandle content
        when ctxInline $ do
          hPutStrLn outHandle "" 
          hPutStrLn outHandle "###LEXML-END###" 
          hFlush outHandle
  hClose outHandle 
  return ()
