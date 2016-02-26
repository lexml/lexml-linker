{-# LANGUAGE FlexibleInstances #-}

module GrammarGenerator where

import qualified Data.Map as M
import qualified Data.Set as S

type VarName = String


data ProdExpr = 
      PE_Lex String 
    | PE_Option ProdExpr 
    | PE_Many ProdExpr 
    | PE_Many1 ProdExpr
    | PE_Choice [ProdExpr] 
    | PE_Concat [ProdExpr] 
    | PE_Empty 
    | PE_Var VarName
    | PE_NonTerminal ProdName [ProdExpr]
    | PE_Bind VarName [ProdExpr] ProdExpr
    | PE_Fail
    deriving (Eq,Ord,Show)

data ProdName = PN String [Int] deriving (Eq,Ord,Show)

data Prod = Prod ProdName [VarName] ProdExpr deriving (Eq,Ord,Show)

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

bind = PE_Bind 

var = PE_Var

nt0 n = PE_NonTerminal (PN n []) []

nt n = PE_NonTerminal (PN n [])

class ProdHead a where
   buildHead :: a -> VarName -> [VarName] -> ProdExpr -> Prod

instance ProdHead String where
   buildHead x v vs = Prod (PN x []) (v : vs)

instance ProdHead ([VarName] -> ProdExpr -> Prod) where
   buildHead x v vs = x (v : vs)

infixl 5 //

x // y = buildHead x y

class ProdMiddle a where
  closeHead :: a -> ProdExpr -> Prod

instance ProdMiddle String where
  closeHead x = Prod (PN x []) []

instance ProdMiddle ([VarName] -> ProdExpr -> Prod) where
   closeHead x = x []



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
        specialize'' PE_Empty = PE_Empty
        specialize'' (PE_Var vn) = 
            case M.lookup vn bs of Nothing -> error $ "free variable found: " ++ vn
                                   Just expr -> expr
        specialize'' (PE_NonTerminal pn ps) = PE_NonTerminal pn $ map specialize'' ps
        specialize'' (PE_Bind vn vs pe) = PE_Choice $ [ specialize' (M.insert vn v bs) pe | v <- vs ]
        specialize'' PE_Fail = PE_Fail

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

freeVarsPL = filter (not . null . snd) . map (\p -> ((case p of Prod pn _ _ -> pn),freeVarsP p))
freeVarsP (Prod _ vs pe) = freeVarsPE pe `S.difference` S.fromList vs

freeVarsPE (PE_Option pe) = freeVarsPE pe
freeVarsPE (PE_Many pe) = freeVarsPE pe
freeVarsPE (PE_Many1 pe) = freeVarsPE pe
freeVarsPE (PE_Choice pl) = foldl S.union S.empty $ map freeVarsPE pl
freeVarsPE (PE_Concat pl) = foldl S.union S.empty $ map freeVarsPE pl
freeVarsPE (PE_Var vn) = S.singleton vn
freeVarsPE (PE_NonTerminal _ pl) = foldl S.union S.empty $ map freeVarsPE pl
freeVarsPE (PE_Bind vn pl pe) = (S.delete vn $ freeVarsPE pe) `S.union` 
                                (foldl S.union S.empty $ map freeVarsPE pl)
freeVarsPE _ = S.empty

lexemes :: ProdExpr -> S.Set String
lexemes (PE_Lex x) = S.singleton x
lexemes (PE_Option x) = lexemes x
lexemes (PE_Many x) = lexemes x
lexemes (PE_Many1 x) = lexemes x
lexemes (PE_Choice pl) = foldl S.union S.empty $ map lexemes pl
lexemes (PE_Concat pl) = foldl S.union S.empty $ map lexemes pl
lexemes (PE_NonTerminal _ pl) = foldl S.union S.empty $ map lexemes pl
lexemes (PE_Bind vn pl pe) = lexemes pe `S.union`  (foldl S.union S.empty $ map lexemes pl)
lexemes _ = S.empty

lexemesPL = foldl S.union S.empty . map (\(Prod _ _ pe) -> lexemes pe)

data NonTerminalRef = NTR String Int deriving (Eq,Ord)

instance Show NonTerminalRef where
  show (NTR name arity) = name ++ "/" ++ show arity

ntrefs :: ProdExpr -> S.Set NonTerminalRef
ntrefs (PE_Option x) = ntrefs x
ntrefs (PE_Many x) = ntrefs x
ntrefs (PE_Many1 x) = ntrefs x
ntrefs (PE_Choice pl) = foldl S.union S.empty $ map ntrefs pl
ntrefs (PE_Concat pl) = foldl S.union S.empty $ map ntrefs pl
ntrefs (PE_NonTerminal pn pl) = S.add (NTR n (length pl)) $ foldl S.union S.empty $ map ntrefs pl
ntrefs (PE_Bind vn pl pe) = ntrefs pe `S.union`  (foldl S.union S.empty $ map ntrefs pl)
ntrefs _ = S.empty

ntrefsL 
