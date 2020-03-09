{-# LANGUAGE FlexibleContexts #-}

module LexML.Linker.Parser (
  LinkerParseError (..), parseReferencias2
) where

import Data.Char
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import LexML.Linker.Decorator (DecorationMap, addURN)
import Data.Typeable
import LexML.Linker.LexerPrim (Token, TokenData(..), tokenDataLength)
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Pos (newPos)
import LexML.URN.Types
import LexML.URN.Show
import qualified LexML.URN.Atalhos as U
import LexML.Linker.ParserBase
-- import LexML.Linker.Regras
import qualified LexML.Linker.Regras2 as R2
import LexML.Linker.RegrasSTF


-- parseReferencias :: (BaseM m, MonadError LinkerParseError m) => [Token] -> m DecorationMap
-- parseReferencias l = execStateT (runPT (many doParse) () "source" l) iniState >>= return . lpsDecorationMap

iniState = LPS {
  lpsDecorationMap = M.empty,
  lpsResolverURL = "http://homologa.lexml.gov.br/urn/URNLEXML",
  lpsContexto = Nothing,
  lpsLogTokens = False,
  lpsLogRegras = False,
  lpsLogOther = False,
  lpsLogMessages = [],
  lpsConstituicaoSimples = False,
  lpsPrevTokens = []
}

{- doParse :: BaseM m => LinkerParserMonad m ()
        doParse = choice ((map (try . parseCase) parseCases ++ [skip])) -}

parseReferencias2 :: [LinkerComponent] -> Maybe String -> URNLexML -> [Token] -> Bool -> (DecorationMap,[String])
parseReferencias2 logComps murlresolver ctx l constituicaoSimples = (lpsDecorationMap res, reverse $ lpsLogMessages res)
  where
    res = execState (fmap join $ runExceptT $ fmap (either (Left . LPE_ParseError) Right) $ runPT (many doParse) () "" l) iniState'''
    iniState''' = iniState'' { lpsConstituicaoSimples = constituicaoSimples }
    iniState'' = foldr enable iniState' logComps
    iniState' = case murlresolver of
      Nothing -> iniState { lpsContexto = Just ctx }
      Just u -> iniState { lpsResolverURL = u, lpsContexto = Just ctx }
    doParse = choice (ignored : (map (try . parseCase2 ctx) (pSTF : R2.parseCases) ++ [skip]))
