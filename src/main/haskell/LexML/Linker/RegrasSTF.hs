module LexML.Linker.RegrasSTF (
  pSTF ) where

import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import LexML.Linker.Decorator (DecorationMap, addURN)
import Data.Typeable
import LexML.Linker.LexerPrim (Token, TokenData(..), tokenDataLength)
import LexML.URN.Utils
import LexML.URN.Types
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Pos (newPos)
import LexML.URN.Types
import LexML.URN.Show
import qualified LexML.URN.Atalhos as U
import LexML.Linker.ParserBase
import Prelude hiding (log)
import qualified LexML.Linker.Municipios as M
import qualified LexML.Linker.Estados as E
import System.Log.Logger

--pSTF :: ParseCase2
--pSTF = fail "failed"

type SelectF = URNLexML -> URNLexML

data Norma = Lei | LeiComplementar | LeiDelegada | Decreto | DecretoLei | ProjetoLei | ProjetoLeiComplementar | MedidaProvisoria |
             Regimento | Resolucao 

log :: String -> LinkerParserMonad ()
log = logRegras


log'' :: String -> LinkerParserMonad ()
log'' msg = do
  lh <- try (eof >> return Nothing) <|> (lookAhead anyToken >>= return . Just)
  log $ msg ++ " (lh: " ++ show lh ++ ")"

pSTF :: ParseCase2
pSTF = constanteI "leg" >>= pSTF'

pSTF' :: Pos -> ParseCase2
pSTF' initial = do
  hifen >> choice [
      constantes [ "FED",  "***" ] >> pEsferaFederal
    , constantes [ "EST" ] >> pEsferaEstadual
    , constantes [ "MUN" ] >> pEsferaMunicipal
    , constantes [ "DIS" ] >> pEsferaDistrital
    ] 
  where
    pEsferaFederal :: ParseCase2
    pEsferaFederal = choice [
          pLei Nothing (Just $ A_Convencionada $ AC_Federal)
      ]
    
    pEsferaEstadual :: ParseCase2
    pEsferaEstadual = fail "not implemented"

    pEsferaMunicipal :: ParseCase2
    pEsferaMunicipal = fail "not implemented"

    pEsferaDistrital :: ParseCase2
    pEsferaDistrital = fail "not implemented"

    pLei :: (Maybe DetalhamentoLocal) -> (Maybe Autoridade) -> ParseCase2
    pLei detLocal (Just autoridade) = do
        tipoNorma <- choice [ constanteI t >> return c | (t,c) <- [
              ("lei",["lei"])
            , ("lcp",["lei","complementar"])
            , ("ldl",["lei","delegada"])
            , ("mpr",["medida","provisoria"])
            , ("dec",["decreto"])
            , ("del",["decreto","lei"])
          ] ]
        hifen
        (_,num) <- pNumeroNorma
        constanteI "ano"
        hifen
        (final,ano) <- pAnoLei
        return $ [(initial,final, U.selecionaLocal (Local Brasil detLocal) . U.selecionaNorma'' tipoNorma num ano Nothing Nothing Nothing (Just autoridade))]
      where
        pAnoLei = pAnoLei1 <|> pAnoLei2

        pAnoLei1 = do
          hifen
          (p,n) <- numero
          let n' = if n < 100 then n+1900 else n
          return (p, fromInteger n)
        
        pAnoLei2 = do
          (p,n) <- numero
          return (p, fromInteger n)
    pLei _ _ = fail "not implemented"
    
    pNumeroNorma :: LinkerParserMonad (Pos,[Integer])  
    pNumeroNorma = do
      (p,n) <- numero
      return (p, [n])
    
    
    numero :: LinkerParserMonad (Pos,Integer)
    numero = lToken f
      where f (Numero n _) = Just n
            f _ = Nothing
