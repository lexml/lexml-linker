{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs 
import Data.Data
import Control.Monad
import LexML.URN.Types hiding (Data)
import LexML.URN.Parser
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
import System.IO
import LexML.MD5

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
            TEXT &= help "Texto de entrada sem mark-up"
          , HXML &= help "Texto de entrada com mark-up (XML/HTML) (default)"
        ] 
    , tipoSaida = enum [
            URNs &= help "Gera lista de URNs reconhecidas (default)"
          , HTML &= help "Gera HTML decorado com links para o resolver do LexML"
--          , XML  &= help "Gera XML decorado com as URNs reconhecidas"
        ] 
    , enderecoResolver = "http://homologa.lexml.gov.br/urn/URNLEXML" &= opt "http://homologa.lexml.gov.br/urn/URNLEXML" &= help "Endereço do LexML Resolver, a string URNLEXML será substituída pela URN. Usado quando com o tipo de saída HTML"
    , contexto = "federal" &= typ "URN" &= opt "federal" &= help "URN Lex ML de contexto. Podem ser usados os apelidos 'federal' e 'senado' para o contexto de leis federais e resoluções do senado, respectivamente" 
    , logaRegras = False &= help "Loga a execução das regras"
    , logaTokens = False &= help "Loga a saída da análise léxica"
    , calculaMD5 = False &= help "Calcula o MD5 da entrada"
    , debug = False &= help "Mostra mensagens de debug"
  }


main = do
  args <- cmdArgs analise
  let logLevel = if debug args then DEBUG else ERROR
  updateGlobalLogger rootLoggerName (setLevel logLevel)
  contexto <- case contexto args of
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
--                XML  -> OT_XLINK
          , loInputType = case tipoEntrada args of
                TEXT -> IT_TEXT
                HXML -> IT_TAGGED
          , loDebugRegras = logaRegras args
          , loDebugTokens = logaTokens args
        }
  inputBS <- case frase args of
              "" -> case entrada args of
                    ""  -> BIO.getContents
                    "-" -> BIO.getContents
                    fname -> BIO.readFile fname
              f -> return  $ fromString f              
  when (calculaMD5 args) $ hPutStrLn stderr $ "MD5: " ++ md5sum inputBS
  let input = toString inputBS
  res <- runExceptT $ linker lo input
  hSetEncoding stdout utf8 
  case res of
    Left err -> return ()
    Right out -> do
      let content = resContent out
      case saida args of
        --"" -> IOUTF.putStr content
        "" -> putStr content
        --"-" -> IOUTF.putStr content
        "-" -> putStr content
        --fname -> IOUTF.writeFile fname content
        fname -> withFile fname WriteMode $ \h -> do
           hSetEncoding h utf8
           hPutStr h content
  


