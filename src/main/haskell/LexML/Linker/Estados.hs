module LexML.Linker.Estados where

import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ( (!) )
import LexML.Linker.ParserBase
import Control.Monad
import Control.Monad.Trans
import LexML.URN.Atalhos
import LexML.Linker.LexerPrim
import Text.Parsec
import System.Log.Logger
import Prelude hiding (log)

-- log :: String -> LinkerParserMonad ()
log msg = return () -- liftIO $ debugM "LexML.Linker.Estados" msg

type ComponenteNome = String
type NomeComposto = [ComponenteNome]

type NomeEstado = NomeComposto
type NomeEstadoNormal = NomeComposto

lista :: [(NomeEstado,NomeEstadoNormal)]
lista = [
      (["acre"],["acre"]) 
    , (["alagoas"],["alagoas"])
    , (["amazonas"],["amazonas"])
    , (["amapá"],["amapa"])
    , (["bahia"],["bahia"])
    , (["ceará"],["ceara"])
    , (["distrito","federal"],["distrito","federal"])
    , (["espírito","santo"],["espirito","santo"]) 
    , (["goiás"],["goias"])
    , (["maranhão"],["maranhao"])
    , (["minas","gerais"],["minas","gerais"])
    , (["mato","grosso","do","sul"],["mato","grosso","sul"])
    , (["mato","grosso"],["mato","grosso"])
    , (["pará"],["para"])
    , (["paraíba"],["paraiba"])
    , (["pernambuco"],["pernambuco"])
    , (["piauí"],["piaui"])
    , (["paraná"],["parana"])
    , (["rio","de","janeiro"],["rio","janeiro"])
    , (["rio","grande","do","norte"],["rio","grande","norte"])
    , (["rondônia"],["rondonia"])
    , (["roraima"],["roraima"])
    , (["rio","grande","do","sul"],["rio","grande","sul"])
    , (["santa","catarina"],["santa","catarina"])
    , (["sergipe"],["sergipe"])
    , (["são","paulo"],["sao","paulo"])
    , (["tocantins"],["tocantins"])    
  ]

newtype Trie a b = Trie (Maybe b,M.Map a (Trie a b)) deriving (Eq,Ord,Show)

unTrie (Trie r) = r

emptyTrie :: Trie a b
emptyTrie = Trie (Nothing,M.empty)

makeTrie :: Ord a => [([a],b)] -> Trie a b
makeTrie = Trie . foldl g (unTrie emptyTrie)
  where
    g r (x,y) = unTrie $ f (Trie r) x y
    f (Trie (_,m)) [] x = Trie (Just x,m)
    f (Trie (l,m)) (k:r) x = Trie (l,M.insert k (f (M.findWithDefault emptyTrie k m) r x) m)

trieEstados :: Trie ComponenteNome NomeEstadoNormal
trieEstados = makeTrie lista


parseTrie :: Show b => Trie String b -> LinkerParserMonad (Pos,Pos,b)
parseTrie (Trie (here,m)) = do
    log $ "parseTrie: start"
    (inicio,td) <- anyTokenData
    log $ "parseTrie: token: " ++ show td
    w <- case td of
              Palavra s -> return $ map toLower s
              Hifen -> return "-"
              _     -> return "" 
    log $ "parseTrie: word: " ++ w
    case M.lookup w m of
      Nothing -> do
        log $ "parseTrie: returning: " ++ show here
        result inicio inicio here
      Just trie -> do
        (_,fim,res) <- parseTrie trie
        log $ "parseTrie: returning2: " ++ show res
        result inicio fim (Just res)
  where
    result _ _ Nothing = fail "Nothing found"
    result i f (Just l)  = return (i,f,l)
    
parseEstado :: LinkerParserMonad (Pos,Pos,NomeEstadoNormal)
parseEstado = parseTrie trieEstados

estadosNormais = S.fromList $ map snd $ lista
