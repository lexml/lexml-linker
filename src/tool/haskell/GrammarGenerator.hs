{-# LANGUAGE FlexibleInstances #-}

module GrammarGenerator where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

type VarName = String

data Lex = L_Literal String | L_TokenType String | L_Not Lex deriving (Eq,Ord)

instance Show Lex where
  show (L_Literal s) = "'" ++ s ++ "'"
  show (L_TokenType s) = s
  show (L_Not l) = "NOT(" ++ show l ++ ")"

data ProdExpr = 
      PE_Lex Lex 
    | PE_Option ProdExpr 
    | PE_Many ProdExpr 
    | PE_Many1 ProdExpr
    | PE_Choice [ProdExpr] 
    | PE_Concat [ProdExpr] 
    | PE_Empty 
    | PE_NonTerminal ProdName 
    | PE_Fail
    deriving (Eq,Ord,Show)

synLevel PE_Empty = 100
synLevel PE_Fail = 100
synLevel (PE_Lex _) = 100
synLevel (PE_NonTerminal _) = 100
synLevel (PE_Option _) = 90
synLevel (PE_Many _) = 90
synLevel (PE_Many1 _) = 90
synLevel (PE_Concat _) = 80
synLevel (PE_Choice _) = 70

parIfNecessary enclosing subExp format 
  | synLevel enclosing > synLevel subExp = format $ "(" ++ pePretty subExp ++ ")"
  | otherwise = format $ pePretty subExp

pePretty :: ProdExpr -> String
pePretty (PE_Lex x) = show x
pePretty enc@(PE_Option pe) = parIfNecessary enc pe (++ "?")
pePretty enc@(PE_Many pe) = parIfNecessary enc pe (++ "*")
pePretty enc@(PE_Many1 pe) = parIfNecessary enc pe (++ "⁺")
pePretty (PE_Choice []) = "⊥"
pePretty (PE_Choice [pe]) = pePretty pe
pePretty enc@(PE_Choice (pe:pes)) = pePretty pe ++ concat [" | " ++ parIfNecessary enc p id | p <- pes ] 
pePretty (PE_Concat []) = "λ"
pePretty (PE_Concat [pe]) = pePretty pe
pePretty enc@(PE_Concat (pe:pes)) = pePretty pe ++ concat [" ∙ " ++ parIfNecessary enc p id | p <- pes ] 
pePretty PE_Empty =  "λ"
pePretty (PE_NonTerminal pn) = pnPretty pn
pePretty PE_Fail = "⊥"

data ProdName = PN String [ProdName] deriving (Eq,Ord,Show)

pnPretty :: ProdName -> String
pnPretty (PN n []) = n
pnPretty (PN n [p]) = n ++ "[" ++  pnPretty p ++ "]"
pnPretty (PN n (p:pns)) = n ++ "[" ++ pnPretty p ++ concat ["," ++ pnPretty pp | pp <- pns ] ++ "]"

infix 6 ::=

data Prod = ProdName ::= ProdExpr deriving (Eq,Ord,Show)

pPretty :: Prod -> String
pPretty (pn ::= pe) = pnPretty pn ++ " ≡ " ++ pePretty pe

infixr 9 ***

infixr 8 |||

x *** (PE_Concat xs) = PE_Concat (x:xs)
x *** pe = PE_Concat [x,pe]

x ||| (PE_Choice xs) = PE_Choice (x:xs)
x ||| pe = PE_Choice [x,pe]

lex = PE_Lex

opt = PE_Option

many = PE_Many

many1 = PE_Many1

empty = PE_Empty

nt0 n = PE_NonTerminal (PN n []) 

nt n ns = PE_NonTerminal (PN n ns)

specialize :: M.Map ProdName Prod -> ProdExpr -> ProdExpr
specialize env = specialize' M.empty
  where
    specialize' bs = specialize''
      where
        specialize'' (PE_Option pe) = PE_Option $ specialize'' pe
        specialize'' (PE_Many pe) = PE_Many $ specialize'' pe
        specialize'' (PE_Many1 pe) = PE_Many1 $ specialize'' pe
        specialize'' (PE_Choice ps) = PE_Choice $ map specialize'' ps
        specialize'' (PE_Concat ps) = PE_Concat $ map specialize'' ps
        specialize'' p@PE_Empty = p
        specialize'' p@(PE_NonTerminal pn) = p
        specialize'' p@PE_Fail = p

optimizeAndRedoIfNecessary f pe =
  let pe' = optimize pe in
    if pe == pe' then f pe' else optimize $ f pe'

optimizeAndRedoIfNecessaryL f ps =
  let ps' = map optimize ps
      notchanged = all (uncurry (==)) $ zip ps ps'
    in if notchanged then f ps' else optimize $ f ps'

optimize :: ProdExpr -> ProdExpr
optimize (PE_Option pe@(PE_Option _)) = optimize pe
optimize (PE_Option pe@(PE_Many _)) = optimize pe
optimize (PE_Option (PE_Many1 pe)) = optimize $ PE_Many pe
optimize (PE_Option PE_Empty) = PE_Empty
optimize (PE_Option PE_Fail) = PE_Empty
optimize (PE_Option pe) = optimizeAndRedoIfNecessary PE_Option pe
optimize (PE_Many (PE_Option pe)) = optimize $ PE_Many pe
optimize (PE_Many pe@(PE_Many _)) = optimize pe
optimize (PE_Many (PE_Many1 pe)) = optimize $ PE_Many pe
optimize (PE_Many PE_Empty) = PE_Empty
optimize (PE_Many PE_Fail) = PE_Empty
optimize (PE_Many pe) = optimizeAndRedoIfNecessary PE_Many pe
optimize (PE_Many1 (PE_Option pe)) = optimize $ PE_Many pe
optimize (PE_Many1 pe@(PE_Many _)) = optimize pe
optimize (PE_Many1 pe@(PE_Many1 _)) = optimize pe
optimize (PE_Many1 PE_Empty) = PE_Empty
optimize (PE_Many1 PE_Fail) = PE_Fail
optimize (PE_Many1 pe) = optimizeAndRedoIfNecessary PE_Many1 pe
optimize (PE_Choice []) = PE_Fail 
optimize (PE_Choice [pe]) = optimize pe
optimize (PE_Choice ps) 
  | any (== PE_Empty) ps = optimize $ PE_Option $ PE_Choice $ filter (/= PE_Empty) ps
  | any (== PE_Fail) ps = optimize $ PE_Choice $ filter (/= PE_Empty) ps
  | any (\x -> case x of PE_Choice _ -> True ; _ -> False) ps = 
          optimize $ PE_Choice $ concatMap (\x -> case x of PE_Choice xs -> xs ; _ -> [x]) ps
  | otherwise = optimizeAndRedoIfNecessaryL PE_Choice ps
optimize (PE_Concat []) = PE_Empty
optimize (PE_Concat [pe]) = optimize pe
optimize (PE_Concat ps) 
  | any (== PE_Fail) ps = PE_Fail
  | any (== PE_Empty) ps = optimize $ PE_Concat $ filter (/= PE_Empty) ps
  | any (\x -> case x of PE_Concat _ -> True ; _ -> False) ps = 
          optimize $ PE_Concat $ concatMap (\x -> case x of PE_Concat xs -> xs ; _ -> [x]) ps
  | otherwise = optimizeAndRedoIfNecessaryL PE_Concat ps
optimize pe = pe                      

optimizeP (pn ::= pe) = pn ::= (optimize pe)

optimizePL = map optimizeP

lexemes :: ProdExpr -> S.Set Lex
lexemes (PE_Lex x) = S.singleton x
lexemes (PE_Option x) = lexemes x
lexemes (PE_Many x) = lexemes x
lexemes (PE_Many1 x) = lexemes x
lexemes (PE_Choice pl) = foldl S.union S.empty $ map lexemes pl
lexemes (PE_Concat pl) = foldl S.union S.empty $ map lexemes pl
lexemes (PE_NonTerminal _) = S.empty
lexemes _ = S.empty

lexemesPL = foldl S.union S.empty . map (\(_ ::= pe) -> lexemes pe)

ntrefs :: ProdExpr -> S.Set ProdName
ntrefs (PE_Option x) = ntrefs x
ntrefs (PE_Many x) = ntrefs x
ntrefs (PE_Many1 x) = ntrefs x
ntrefs (PE_Choice pl) = foldl S.union S.empty $ map ntrefs pl
ntrefs (PE_Concat pl) = foldl S.union S.empty $ map ntrefs pl
ntrefs (PE_NonTerminal pn) = ntrefsPN pn 
ntrefs _ = S.empty

ntrefsPN pn = S.singleton pn 

type GrammarWriter a = Writer [Prod] a

grammar = snd . runWriter

infix 6 #=

(#=) :: String -> ProdExpr -> GrammarWriter ()
n #= pe = tell [PN n [] ::= pe]


infix 6 ##=

(##=) :: ProdName -> ProdExpr -> GrammarWriter ()
pn ##= pe = tell [pn ::= pe]

