module LexML.URN.Utils where

import LexML.URN.Types
import Data.Maybe
import Data.Either

algumaData :: URNLexML -> Maybe Data
algumaData (URNLexML _ (Documento _ _ (Descritor desc _ _)) (mversao) _ _) = 
    case desc of
      TD_Datas ds _ -> fromDatas ds
      TD_Apelido (Just (DatasOuAno (Left ds))) _ -> fromDatas ds
      _ -> case mversao of
             Nothing -> Nothing
             Just v -> fromVersao v
  where
    fromDatas (Datas e) = Just $ either id fst e
    fromVersao (Versao (TV_Datas ds) _) = fromDatas ds
    fromVersao (Versao (TV_VersaoVigenteEm d) _) = Just d
    fromVersao (Versao (TV_VersaoEficazEm d) _) = Just d
    fromVersao (Versao (TV_VersaoConsultadaEm d) _) = Just d 
    fromVersao _ = Nothing

type Esfera = AutoridadeConvencionada

selecionaEsferaSuperior :: Esfera -> URNLexML -> URNLexML
selecionaEsferaSuperior _ u@(URNLexML (Local Brasil Nothing) _ _ _ _) = u
selecionaEsferaSuperior esf u@(URNLexML (Local Brasil _) (Documento aut td desc) versao forma fragmento) =
  let aut' = case aut of
                A_Convencionada ac | esf < ac -> A_Convencionada esf
                _ -> aut
      in URNLexML (Local Brasil Nothing) (Documento aut' td desc) versao forma fragmento

autoridadeConvencionada (URNLexML _ (Documento aut _ _ ) _ _ _ ) = autoridadeConvencionada' aut

autoridadeConvencionada' (A_Convencionada ac) = Just ac
autoridadeConvencionada' (A_Normal [SJ_Instituicao (Instituicao (Nome nome)) _ _]) =
  case nome of
    ["supremo","tribunal","federal"] -> Just AC_Federal
    ["senado","federal"] -> Just AC_Federal
    ["tribunal","superior","trabalho"] -> Just AC_Federal
    ["tribunal","superior","eleitoral"] -> Just AC_Federal
    ["superior","tribunal","justica"] -> Just AC_Federal
    _ -> Nothing
autoridadeConvencionada' _ = Nothing

substituiAutoridadeConvencionada ac (URNLexML local (Documento autoridade td desc) versao forma fragmento) =
    URNLexML local (Documento autoridade' td desc) versao forma fragmento
  where
    autoridade' = case autoridade of
      A_Convencionada _ -> autoridade
      _ -> A_Convencionada ac

ehAutoridadeConvencionada (URNLexML _ (Documento (A_Convencionada _) _ _ ) _ _ _ ) = True
ehAutoridadeConvencionada _ = False 
