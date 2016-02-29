module ANTLR4Generator where

import GrammarGenerator
import Data.Char (toUpper)
import qualified Data.Map as M

escapeApos = concatMap (\c -> if c == '\'' then "X1X" else [c])

pnANTLR :: ProdName -> String
pnANTLR (PN n []) = escapeApos n
pnANTLR (PN n [p]) = escapeApos n ++ "X2X" ++ pnANTLR p ++ "X3X"
pnANTLR (PN n (p:pns)) = escapeApos n ++ "X2X" ++ pnANTLR p ++ (concatMap (\p' -> "X4X" ++ pnANTLR p') pns) ++ "X3X"

literalToRule = M.fromList [
    (",", "virgula"),
    ("-", "hifen"),
    (".", "ponto"),
    ("/", "barra"),
    (";", "pontoevirgula") 
  ]

lexANTLR (L_Literal s) = 
  case M.lookup s literalToRule of
    Just n -> n
    Nothing -> "PALAVRA_" ++ map toUpper s
lexANTLR (L_TokenType t) = t
lexANTLR (L_Not l) = "(~ " ++ lexANTLR l ++ " )"

peANTLR (PE_Lex l) = lexANTLR l
peANTLR (PE_Option pe) = "(" ++ peANTLR pe ++ ")?"
peANTLR (PE_Many pe) = "(" ++ peANTLR pe ++ ")*"
peANTLR (PE_Many1 pe) = "(" ++ peANTLR pe ++ ")+"
peANTLR (PE_Choice []) = error "empty choice" 
peANTLR (PE_Choice [a]) = error $ "singleton choice: " ++ show a
peANTLR (PE_Choice (p:pes)) = "(" ++ peANTLR p ++ concatMap (\p' -> " | " ++ peANTLR p') pes ++ ")"
peANTLR (PE_Concat (p:pes)) = "(" ++ peANTLR p ++ concatMap (\p' -> " " ++ peANTLR p') pes ++ ")"
peANTLR PE_Empty = ""
peANTLR (PE_NonTerminal pn) = pnANTLR pn
peANTLR PE_Fail = " {false}? "

prodANTLR (pn ::= pe) = pnANTLR pn ++ " : " ++ peANTLR pe ++ ";"

prodsANTLR = unlines . map prodANTLR
