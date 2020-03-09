{-# LANGUAGE DeriveDataTypeable #-}
module LexML.Linker.ParserBase where

import Data.Char
import Data.Maybe
import Control.Monad hiding (fail)
import Control.Monad.Except hiding (fail)
import Control.Monad.Trans hiding (fail)
import Control.Monad.Identity hiding (fail)
import Control.Monad.State as CMS hiding (fail)
import Control.Monad.Writer hiding (fail)
import LexML.Linker.Decorator (DecorationMap, addURN)
import Data.Typeable
import LexML.Linker.LexerPrim (Token, TokenData(..), tokenDataLength, Genero(..))
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Pos (newPos)
import LexML.URN.Types
import LexML.URN.Show
import qualified LexML.URN.Atalhos as U
import qualified Data.Set as S
import Prelude hiding (log, fail)
import Control.Monad.Fail

logCheck :: (LinkerParserState -> Bool) -> String -> LinkerParserMonad ()
logCheck logf msg = lift $ do
  st <- get
  if logf st then put (st { lpsLogMessages = msg : lpsLogMessages st }) else return ()

logTokens = logCheck lpsLogTokens

logRegras = logCheck lpsLogRegras

logOther = logCheck lpsLogOther

data LinkerParseError = LPE_ParseError ParseError | LPE_Other String deriving (Show,Typeable)

data LinkerComponent = LC_Tokens | LC_Regras | LC_Other deriving (Eq,Ord,Show)

enable LC_Tokens st = st { lpsLogTokens = True }
enable LC_Regras st = st { lpsLogRegras = True }
enable LC_Other st = st { lpsLogOther = True }

data LinkerParserState = LPS {
        lpsDecorationMap :: DecorationMap
      , lpsResolverURL :: String
      , lpsContexto :: Maybe URNLexML
      , lpsLogTokens :: Bool
      , lpsLogRegras :: Bool
      , lpsLogOther :: Bool
      , lpsLogMessages :: [String]
      , lpsConstituicaoSimples :: Bool
      , lpsPrevTokens ::  [TokenData]
    } deriving (Eq,Show)

--type LinkerParserMonad m = ParsecT [Token] () (StateT LinkerParserState m)

type LinkerParserMonad = ParsecT [Token] () (ExceptT LinkerParseError (CMS.State LinkerParserState))

type Pos = (Int,Int,Int)

type ParseCaseResult = [(Pos,Pos,URNLexML)]
type SingleParseCaseResult2 = (Pos,Pos,URNLexML -> URNLexML)
type ParseCaseResult2 = [SingleParseCaseResult2]
type ParseCase = LinkerParserMonad ParseCaseResult
type ParseCase2 = LinkerParserMonad ParseCaseResult2

{- instance Error LinkerParseError where
  noMsg = strMsg ""
  strMsg = LPE_Other
-}

getUrnContexto :: LinkerParserMonad (Maybe URNLexML)
getUrnContexto = lift $ gets lpsContexto

shouldParseConstituicaoSimples :: LinkerParserMonad Bool
shouldParseConstituicaoSimples = lift $ gets lpsConstituicaoSimples

ponto :: LinkerParserMonad Pos
ponto = fmap fst $ lToken maybePonto
  where
    maybePonto :: TokenData -> Maybe ()
    maybePonto Ponto = Just ()
    maybePonto _ = Nothing

indicadorOrdinal :: LinkerParserMonad (Pos,Genero)
indicadorOrdinal = lToken maybeIndicador
  where
    maybeIndicador (IndicadorOrdinal g) = Just g
    maybeIndicador (Palavra "o") = Just Masc
    maybeIndicador _ = Nothing

addurn :: String -> (Int,Int,Int) -> (Int,Int,Int) -> LinkerParserMonad ()
addurn urn (x1,y1,_) (x2,y2,l2) = lift $ modify $ \s -> s { lpsDecorationMap =  addURN x1 y1 x2 (y2+l2) urn $ lpsDecorationMap s }

replace r _ _ [] = reverse r
replace r [] subst text = reverse r ++ subst ++ text
replace r pat@(p:ps) subs@(s:ss) (t:text) | p == t = 
    maybe (replace (t:r) pat subs text) id (replace' (s:r) ps ss text)
  where
    replace' r pat@(p:ps) subs@(s:ss) (t:text) | p == t = replace' (s:r) ps ss text
    replace' r [] subs text = Just $ reverse r ++ subs ++ text
    replace' _ _ _ _ = Nothing
replace r pat subs (t:text) = replace (t:r) pat subs text

skip :: LinkerParserMonad ()
skip = lToken (const (Just ())) >> return ()

lToken :: (TokenData -> Maybe a) -> LinkerParserMonad ((Line, Column, Int), a)
lToken f = do
  ((x,y,l,d),tok) <- tokenPrim show (\ _ ((x,y),_) _ -> newPos "" x y) (\ ((x,y),d) -> fmap ((,) (x,y,tokenDataLength d,d)) (f d))
  modify $ \s -> s { lpsPrevTokens = d : lpsPrevTokens s }
  return ((x,y,l),tok)

anyTokenData :: LinkerParserMonad (Pos, TokenData)
anyTokenData = lToken Just

printCurrentTag :: LinkerParserMonad ()
printCurrentTag = try $ do
  (pos,token) <- lToken Just
  logRegras $ "pos = " ++ show pos ++ ", token = " ++ show token
  fail "ops"

parseCase p = do
  l <- p
  mapM_ (\ (p1,p2,s) -> addurn (urnShow s) p1 p2) l

parseCase2 ctx p = do
  l <- p
  mapM_ ( \(p1,p2,u) -> addurn (urnShow (u ctx)) p1 p2) l

anyPalavra :: LinkerParserMonad (Pos,String)
anyPalavra = lToken f
  where
    f (Palavra s) = return s
    f _ = fail "expecting palavra"

palavraFilter :: (String -> Bool) -> LinkerParserMonad (Pos,String)
palavraFilter h = lToken f
  where
    f (Palavra s) | h s = return s
    f _ = fail "expecting palavra"

palavra :: Parsec String () a -> LinkerParserMonad ((Line, Column, Int), a)
palavra f = lToken palavra' 
  where 
    palavra' (Palavra s) = 
      case parse f (show s) s of
         Left err -> fail $ show err
         Right r  -> return $ r
    palavra' _ = fail $ "Palavra expected"

palavraI f = lToken palavra' 
  where 
    palavra' (Palavra s) = 
      case parse f (show s) (map toLower s) of
         Left err -> fail $ show err
         Right r  -> return r
    palavra' _ = fail $ "PalavraI expected"

constante :: String -> LinkerParserMonad (Int,Int, Int)
constante s = fmap fst $ lToken constante'
  where
    constante' (Palavra p) 
      | p == s = return ()
    constante' _ = fail $ "Expected " ++ show s

constantes :: [String] -> LinkerParserMonad (Int,Int,Int)
constantes s = fmap fst $ lToken constantes'
  where
    constantes' (Palavra p)
      | p `elem` s = return ()
    constantes' _ = fail $ "Expected " ++ show s

constanteI :: String -> LinkerParserMonad (Int,Int, Int)
constanteI s = fmap fst $ lToken constante'
  where
    constante' (Palavra p) 
      | map toLower p == map toLower s = return ()
    constante' _ = fail $ "Expected " ++ show s

nomeProprio :: String -> LinkerParserMonad (Int,Int, Int)
nomeProprio s@(c1:cs1) = fmap fst $ lToken constante'
  where
    constante' (Palavra "") = fail $ "Expected " ++ s
    constante' (Palavra (c2:cs2)) 
      | c1 == c2 && map toLower cs1 == map toLower cs2 = return ()
    constante' _ = fail $ "Expected " ++ show s

constantesI2 :: [String] -> LinkerParserMonad ((Int,Int,Int),(Int,Int,Int))
constantesI2 l = do
  r <- mapM (try . constanteI) l
  return (head r, last r)

constantesI :: [String] -> LinkerParserMonad (Int,Int,Int)
constantesI = fmap fst . constantesI'

constantesI' :: [String] -> LinkerParserMonad ((Int,Int,Int),String)
constantesI' l = lToken constante'
  where 
    s = S.fromList [map toLower s | s <- l]
    constante' (Palavra p) 
      | map toLower p `S.member` s = return (map toLower p)
    constante' _ = fail $ "Expected " ++ show s

parseLookup :: [(String,a)] -> LinkerParserMonad ((Int,Int,Int),a)
parseLookup l = lToken f
  where f (Palavra s) = M.lookup (map toLower s) m
        f _ = Nothing
        m = M.fromList l

simbolo :: TokenData -> LinkerParserMonad Pos
simbolo td = fmap fst $ lToken f
  where
    f td' = if td == td' then return () else fail ("Expecting "  ++ show td)

virgula :: LinkerParserMonad Pos
virgula = simbolo Virgula

pontoevirgula :: LinkerParserMonad Pos
pontoevirgula = simbolo PontoeVirgula   

barra :: LinkerParserMonad Pos
barra = simbolo Barra 

combine :: ParseCaseResult2 -> SingleParseCaseResult2 -> ParseCaseResult2
combine leftl right@(_,fim,urn) = res
  where
    leftl' = [(i,f,u . urn) | (i,f,u) <- leftl]
    res = if null leftl then [right] else leftl''
    ((ilast,_,urnlast):beforeLast) = reverse leftl'
    leftl'' = reverse ((ilast,fim,urnlast):beforeLast)

combineM :: MonadFail m => m ParseCaseResult2 -> m ParseCaseResult2 -> m ParseCaseResult2
combineM leftM rightM = do
  leftl <- leftM
  (right:rl) <- rightM
  return $ (leftl `combine` right) ++ rl

combineM' :: LinkerParserMonad ParseCaseResult2 -> LinkerParserMonad SingleParseCaseResult2 -> (Maybe (URNLexML -> URNLexML)) -> LinkerParserMonad ParseCaseResult2
combineM' leftM rightM def = try case2 <|> case1 
  where
    case1 = do
--      parseBaseLog $ "                       combineM': parsing leftM"
      leftl <- leftM
--      parseBaseLog $ "                       combineM': parsing rightM"
      mright <- option Nothing (fmap Just (try rightM))
--      parseBaseLog $ "                       combineM': right parsed. returning. isJust = " ++ show (isJust mright)
      case mright of Nothing -> case def of
                                  Nothing -> return leftl
                                  (Just h) -> return [(i,f, u {- . h -} ) | (i,f,u) <- leftl]
                     Just r  -> return $ leftl `combine` r
    case2 = rightM >>= \r -> return [r] 

failIfEmpty msg p = p >>= \l -> if null l then fail msg else return l

ignored :: LinkerParserMonad ()
ignored = lToken f >> return ()
  where
    f (Ignored _) = return ()
    f _ = fail "expecting ignored"

hifen :: LinkerParserMonad (Int,Int,Int)
hifen = fmap fst $ lToken f
  where f Hifen = return ()
        f _ = fail "expected hifen"

prev_tokens :: LinkerParserMonad [TokenData]
prev_tokens = lift $ do
 s <- get
 return $ lpsPrevTokens s
