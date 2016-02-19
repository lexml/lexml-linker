{-# LANGUAGE DeriveDataTypeable #-}
module LexML.Linker.LexerPrim where

import Text.HTML.TagSoup
--import Text.HTML.TagSoup.Parser
import qualified Data.Foldable as F
import Data.Maybe
import Data.Char
import Data.Typeable
import Control.Monad.Except
import qualified Data.Set as S
import Data.Word (Word8)
import Codec.Binary.UTF8.String (encode)

data LexerState = 
  LexerState {
    lsPrevPos :: Maybe TokenPos,
    lsCurrentPos :: Int,
    lsStream :: [(Int,String)],
    lsPrevCharBytes :: [Word8],
    lsPrevChars :: String
  } deriving (Show)

type AlexInput = LexerState

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte ls =
  case lsPrevCharBytes ls of
    [] -> case lsStream ls of
               ((_,"") : rest) -> alexGetByte (ls { lsCurrentPos = 0, lsStream = rest })
               ((p,c:cs) : rest) -> 
                  let (b:bs) = encode [c] in
                    Just (b, ls { lsCurrentPos = lsCurrentPos ls + 1, 
                                  lsPrevPos = Just $ fromMaybe (p,lsCurrentPos ls) id (lsPrevPos ls), 
                                  lsPrevChars = c : lsPrevChars ls, 
                                  lsPrevCharBytes = bs,
                                  lsStream = (p,cs) : rest})
               [] -> Nothing
    (b:bs) -> Just (b,ls { lsPrevCharBytes = bs})
      
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = head . lsPrevChars

makeLexStream tags =  
    LexerState {
      lsCurrentPos = 0,
      lsPrevChars = [],
      lsPrevCharBytes = [],
      lsPrevPos = Nothing,
      lsStream = [(p,text) | (p, TagText text) <- zip [0 ..] tags ]
    }


extract n ls = (reverse . take n . lsPrevChars $ ls, ls { lsPrevChars = [], lsPrevPos = Nothing } )

skip _ ls = ls

data Genero = Masc | Fem deriving (Eq,Ord,Show,Typeable)

type Token = (TokenPos,TokenData) 

type TokenPos = (Int,Int)

data TokenData =
    Numero !Integer !String
  | Palavra !String
  | Ponto
  | Virgula
  | Barra
  | PontoeVirgula
  | Hifen
  | IndicadorOrdinal !Genero
  | Paragrafos
  | Paragrafo
  | Ordinal !Integer !String
  | Simbolo !Char
  | Ignored !Int
  deriving (Eq,Ord,Show,Typeable)

tokenDataLength :: TokenData -> Int
tokenDataLength (Numero _ s) = length s
tokenDataLength (Palavra s) = length s
tokenDataLength Paragrafos = 2
tokenDataLength (Ignored n) = n
tokenDataLength _ = 1


type Action = String -> Maybe TokenData

whitespace :: Action
whitespace = const Nothing

numero :: Action
numero s = case filter isDigit s of
              "" -> Nothing
              s' -> Just $ Numero (read s') s

palavra :: Action
palavra = Just . Palavra

ponto :: Action
ponto = Just . const Ponto

barra :: Action
barra = Just . const Barra

ordinal :: Action
ordinal s = case filter isDigit s of
              "" -> Nothing
              s' -> Just $ Ordinal (read s') s

virgula :: Action
virgula = Just . const Virgula

pontoeVirgula :: Action
pontoeVirgula = Just . const PontoeVirgula

hifen :: Action
hifen = Just . const Hifen

indicadorOrdinal :: Action
indicadorOrdinal "\xba" = Just $ IndicadorOrdinal Masc
indicadorOrdinal "\xaa" = Just $ IndicadorOrdinal Fem
indicadorOrdinal "\xb0" = Just $ IndicadorOrdinal Masc

paragrafos :: Action
paragrafos = Just . const Paragrafos

paragrafo :: Action
paragrafo = Just . const Paragrafo

simbolo :: Action
simbolo = Just . Simbolo . head

extractToken :: Int -> Action -> LexerState -> (Maybe Token,LexerState)
extractToken len action ls = ( (,) pos <$> action tokenText, ls')
  where
    pos = prevPos ls
    (tokenText,ls') = extract len ls

prevPos :: LexerState -> (Int,Int)
prevPos ls = fromJust $ lsPrevPos ls -- fst . head  . lsStream $ ls, lsCurrentPos ls)

currentPos :: LexerState -> (Int,Int)
currentPos ls = 
  let ((p,_):_) = lsStream ls in (p,lsCurrentPos ls)

newtype LexError = LexError (Maybe String,Maybe TokenPos) deriving (Eq,Ord,Typeable)

instance Show LexError where
  show (LexError (Nothing,p)) = show (LexError (Just "Erro não-específico na análise léxica.",p))
  show (LexError (Just m, Nothing)) = m
  show (LexError (Just m, Just p)) = m ++ " em " ++ show p

{- instance Except LexError where
  noMsg = LexError (Nothing, Nothing)
  strMsg s = LexError (Just s,Nothing)
  -}

