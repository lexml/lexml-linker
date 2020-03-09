{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module LexML.Linker (
  LinkerError (..),
  linker,
  LinkerOptions (..),
  RefContexto (..),
  Result (..),
  InputType (..),
  OutputType (..),
  contextoFederal,
  defaultLinkerOptions,
  refContextoFederal
 ) where

import Text.HTML.TagSoup 
import Control.Monad.Identity
import Control.Monad.Except
import LexML.Linker.LexerPrim (Token, LexError)
import LexML.Render (render)
import LexML.URN.Show
import LexML.URN.Utils (urnHasContextoFederal)
import LexML.Linker.Lexer (tokenStream)
import LexML.Linker.Decorator (DecorationMap, decorate, makeTagsAHRef, makeTagsXLink)
import LexML.Linker.Parser (LinkerParseError, parseReferencias2)
import LexML.Linker.ParserBase (LinkerComponent (..))
import LexML.Linker.HtmlCleaner (cleanEmptyAnchors)
import System.IO
import Control.Monad
import Control.Monad.Except
import Data.Typeable
import Data.Map ( (!) )
import qualified Data.Map as M
import LexML.URN.Types
import LexML.Linker.URLTransform (rewriteAnchors)
import Prelude hiding (log)
import System.Log.Logger
import qualified Data.Map as M
import qualified Data.Set as S

linkerLog msg = liftIO $ debugM "LexML.Linker" msg
linkerNotice msg = liftIO $ noticeM "LexML.Linker" msg

data LinkerError = LE_Lexer LexError | LE_Parser LinkerParseError | LE_Other String deriving (Show,Typeable)

myParseOptions = parseOptions { optTagPosition = False }

mapError :: Monad m => (e1 -> e2) -> ExceptT e1 m a -> ExceptT e2 m a
mapError g = mapExceptT (\m -> do { res <- m ; return $ either (Left . g) Right res })

type NomeContexto = String
type SourceName = String

data Result = Result { resPartial :: Bool, resContent :: String } deriving (Show)

data RefContexto = RC_Nome NomeContexto | RC_URNLexML URNLexML | RC_AutoridadeLocal Autoridade (Maybe DetalhamentoLocal) deriving (Eq,Ord,Show)

data InputType = IT_TAGGED | IT_TEXT deriving (Eq,Ord,Show)
data OutputType = OT_AHREF | OT_XLINK | OT_URNS deriving (Eq,Ord,Show)

refContextoFederal :: RefContexto -> Bool
refContextoFederal (RC_Nome "federal") = True
refContextoFederal (RC_URNLexML urn) = urnHasContextoFederal urn
refContextoFederal (RC_AutoridadeLocal _ _ ) = False -- (aut :: Autoridade) (mdet :: Maybe DetalhamentoLocal)) = False

data LinkerOptions = LO {
    loContext :: RefContexto
  , loBaseUrl :: Maybe String
  , loTokenLimit :: Maybe Int
  , loResolverUrl :: Maybe String
  , loInputType :: InputType
  , loOutputType :: OutputType
  , loDebugRegras :: Bool
  , loDebugTokens :: Bool
  , loDebugOther :: Bool
  , loConstituicaoSimples :: Bool
  } deriving (Eq,Ord,Show)   

defaultLinkerOptions = LO {
    loContext = RC_Nome "federal"
  , loBaseUrl = Nothing
  , loTokenLimit = Nothing
  , loResolverUrl = Nothing
  , loInputType = IT_TAGGED
  , loOutputType = OT_AHREF
  , loDebugRegras = False
  , loDebugTokens = False
  , loDebugOther = False
  , loConstituicaoSimples = False
  }

toComponents lo = 
    includeIf loDebugTokens LC_Tokens $
    includeIf loDebugRegras LC_Regras $
    includeIf loDebugOther LC_Other []
  where 
    includeIf f c l 
      | f lo = c : l
      | otherwise = l

makeContext :: RefContexto -> URNLexML
makeContext (RC_Nome nomeContexto) = contextos ! nomeContexto
makeContext (RC_AutoridadeLocal autoridade mlocal) = 
      URNLexML (Local Brasil mlocal) 
               (Documento autoridade
                          (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["lei"]))) Nothing) 
                          (Descritor (TD_Datas (Datas $ Left (Data 2008 10 5)) (Just (ID_Ids [IdDocumento $ NormalID $ "123"]))) [] Nothing)) 
               Nothing 
               Nothing 
               Nothing
makeContext (RC_URNLexML urn) = urn

linker :: LinkerOptions -> String -> ExceptT LinkerError IO Result
linker lo source = do
  let context = makeContext (loContext lo)
      tags = case loInputType lo of
               IT_TAGGED -> cleanEmptyAnchors $ parseTagsOptions myParseOptions source
               IT_TEXT   -> [TagText source]
  linkerNotice $ "starting: context = " ++ show context 
--  linkerLog "tags: "
--  linkerLog $ concat ["    [" ++ show n ++ "] " ++ show t ++ "\n" | (n,t) <- zip [0 ..] tags ]
  tokens' <- mapError LE_Lexer $ tokenStream tags
  let initialTokenLength = length tokens'
  let (tokens,partial) = 
        case loTokenLimit lo of 
          Nothing -> (tokens',False)
          Just limit -> 
            let (x,y) = splitAt limit tokens' in (x,not $ null y)
  linkerNotice $ "linker: initial token length = " ++ show initialTokenLength ++ ", final token length = " ++ show (length tokens)
  if loDebugTokens lo then linkerLog $ concat $ ["    " ++ show t ++ "\n" | t <- tokens ] else return ()
  let resolverURL = case loResolverUrl lo of
        Nothing -> "http://homologa.lexml.gov.br/urn/URNLEXML"
        Just u -> u
      logComps = toComponents lo
      (modMap,logMsgs) = parseReferencias2 logComps (loResolverUrl lo) context tokens (loConstituicaoSimples lo)
  linkerLog $ "Linker log: "
  linkerLog $ unlines logMsgs
  linkerLog $ "modMap: " ++ show modMap
  res <- case loOutputType lo of
    OT_URNS -> return $ unlines $ S.toAscList $ S.fromList [ urn | m <- M.elems modMap, (_,_,urn) <- M.elems m ]
    _ -> do let makeTags = case loOutputType lo of
                  OT_AHREF -> makeTagsAHRef resolverURL
                  OT_XLINK -> makeTagsXLink
                decoratedTags = decorate makeTags modMap tags
            linkerLog $ "decoratedTags:" 
            linkerLog $ concat ["    " ++ show t | t <- decoratedTags ]
            let transformedTags = case loBaseUrl lo of
                  Just base | loOutputType lo == OT_AHREF -> rewriteAnchors base decoratedTags
                  _   -> decoratedTags
            linkerLog "transformedTags: "
            linkerLog $ concat [ "    " ++ show t ++ "\n" | t <- transformedTags ]
            return $ render transformedTags
  return $ Result {
        resContent = res
      , resPartial = partial
    }

contextos = M.fromList [ 
      ("federal", contextoFederal)
    , ("senado", contextoSenado)
  ]


contextoFederal = 
      URNLexML (Local Brasil Nothing) 
               (Documento (A_Convencionada AC_Federal) 
                          (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["lei"]))) Nothing) 
                          (Descritor (TD_Datas (Datas $ Left (Data 2008 10 5)) (Just (ID_Ids [IdDocumento $ NormalID $ "123"]))) [] Nothing)) 
               Nothing 
               Nothing 
               Nothing

contextoSenado = 
      URNLexML (Local Brasil Nothing) 
               (Documento (A_Normal [SJ_Instituicao (Instituicao $ Nome ["senado","federal"]) [] Nothing]) 
                          (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["resolucao"]))) Nothing) 
                          (Descritor (TD_Datas (Datas $ Left (Data 2008 10 5)) (Just (ID_Ids [IdDocumento $ NormalID $ "123"]))) [] Nothing)) 
               Nothing 
               Nothing 
               Nothing
