{

{-# LANGUAGE FlexibleContexts #-}
module LexML.Linker.Lexer where

import LexML.Linker.LexerPrim
import Text.HTML.TagSoup
--import Text.HTML.TagSoup.Parser
import qualified Data.Foldable as F
import Data.Maybe
import Data.Typeable
import Control.Monad.Except

}

$whitechar = [ \t\n\r\f\v]

$digit = [0-9]

$large     = [A-Z\xc0-\xdd] 
$small     = [a-z\xe0-\xff]
$alpha     = [$small $large]
$indicOrdinalMasc = [\xba]
$indicOrdinalFem = [\xaa]
$deg = [\xb0]
$indicOrdinal = [$indicOrdinalMasc $indicOrdinalFem $deg]
$paragrafo = [\xa7] 

@Numero =  $digit+
-- @NumeroRomano = ( "M"+ ( "CM" | "D" "C"{1,3} | "CD" | "C"{0,3} ) ( "XC" | "L" "X"{1,3} | "XL" | "X"{0,3} ) ( "IX" | "V" "I"{1,3} | "IV" | "I"{0,3} ) ) |
--                ( ( "CM" | "D" "C"{1,3} | "CD" | "C"{1,3} ) ( "XC" | "L" "X"{1,3} | "XL" | "X"{0,3} ) ( "IX" | "V" "I"{1,3} | "IV" | "I"{0,3} ) ) |
--                ( ( "XC" | "L" "X"{1,3} | "XL" | "X"{1,3} ) ( "IX" | "V" "I"{1,3} | "IV" | "I"{0,3} ) ) |
--                ( ( "IX" | "V" "I"{1,3} | "IV" | "I"{1,3} ) ) 

@Palavra = $alpha+

:- 

<0> $white+                     { whitespace }
<0> @Palavra                    { palavra }
<0> @Numero                     { numero }
-- <0> [0-9]+ ("." [0-9]{3})*      { numero }
<0> "1o"                        { ordinal }
<0> "."                         { ponto }
<0> ","                         { virgula }
<0> ";"                         { pontoeVirgula}
<0> "-"                         { hifen }
<0> "/"                         { barra }
<0> $indicOrdinal               { indicadorOrdinal }
<0> $paragrafo $paragrafo       { paragrafos }
<0> $paragrafo                  { paragrafo }
<0> .                           { simbolo }

{

--tokenStream :: [Tag] -> Either LexError [Token]
tokenStream tags = f ls >>= return -- .  filterIgnoredWords
-- tokenStream tags = f ls
  where
    ls = makeLexStream tags
    f ls = case alexScan ls 0 of
             AlexEOF -> return []
             AlexError inp -> throwError $ LexError (Nothing,Just $ currentPos inp)
             AlexSkip inp numSkip -> f (skip numSkip inp)
             AlexToken inp len action -> case extractToken len action inp of
                  (Nothing,inp') -> f inp'
                  (Just t,inp') -> f inp' >>= \l -> return (t:l)

}
