module LexML.URN.Atalhos where

import LexML.URN.Types
import Data.Maybe (fromJust,maybe)
import qualified Data.Map as M
import Data.List
import System.IO.Unsafe (unsafePerformIO)

leiFederal :: [Integer] -> Dia -> Mes -> Ano -> URNLexML
leiFederal numLei dia mes ano = URNLexML (Local Brasil Nothing) (Documento (A_Convencionada AC_Federal)  (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["lei"]))) Nothing) (Descritor (TD_Datas (Datas (Left $ Data ano mes dia)) (Just $ ID_Ids [IdDocumento $ makeNormalID numLei])) [] Nothing) ) Nothing Nothing Nothing

normaFederal :: [String] -> [Integer] -> Dia -> Mes -> Ano -> URNLexML
normaFederal tipo numLei dia mes ano = URNLexML (Local Brasil Nothing) (Documento (A_Convencionada AC_Federal)  (TipoDocumento1 (STD1_Norma (TipoNorma (Nome tipo))) Nothing) (Descritor (TD_Datas (Datas (Left $ Data ano mes dia)) (Just $ ID_Ids [IdDocumento $ makeNormalID numLei])) [] Nothing) ) Nothing Nothing Nothing

leiFederal' :: [Integer] -> Ano -> URNLexML
leiFederal' numLei ano = URNLexML (Local Brasil Nothing) (Documento (A_Convencionada AC_Federal)  (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["lei"]))) Nothing) (Descritor (TD_Ano ano (ID_Ids [IdDocumento $ makeNormalID numLei])) [] Nothing) ) Nothing Nothing Nothing

normaFederal' :: [String] -> [Integer] -> Ano -> URNLexML
normaFederal' tipo numLei ano = URNLexML (Local Brasil Nothing) (Documento (A_Convencionada AC_Federal)  (TipoDocumento1 (STD1_Norma (TipoNorma (Nome tipo))) Nothing) (Descritor (TD_Ano ano (ID_Ids [IdDocumento $ makeNormalID numLei])) [] Nothing) ) Nothing Nothing Nothing


leiFederalApelido :: [String] -> URNLexML
leiFederalApelido nl = URNLexML (Local Brasil Nothing) (Documento (A_Convencionada AC_Federal)  (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["lei"]))) Nothing) (Descritor (TD_Apelido Nothing (ApelidoDocumento (Nome nl))) [] Nothing) ) Nothing Nothing Nothing

ordemTCF tcf = fromJust $ M.lookup tcf $ M.fromList [
    (TCF_Parte,[0,0]),
    (TCF_Livro,[0,1]), 
    (TCF_Titulo,[0,2]),
    (TCF_Capitulo,[0,3]),
    (TCF_Secao,[0,4]),
    (TCF_SubSecao,[0,5]),
    (TCF_AgrupamentoHierarquico,[0,6]),
    (TCF_Artigo,[1,0]),
    (TCF_Caput,[1,1]),    
    (TCF_Paragrafo,[1,1]),
    (TCF_Inciso,[1,2]),
    (TCF_Alinea,[1,3]),  
    (TCF_Item,[1,4]),       
    (TCF_DispositivoGenerico,[1,5]),
    (TCF_Aspas,[1,6])
  ]

splitFragmento tcf comps = (bef,middle,comps'')
  where
   (bef,comps') = span (\ (CompFragmento tcf' _) -> ordemTCF tcf' < ordemTCF tcf) comps
   (middle,comps'') = case comps' of
       (c@(CompFragmento tcf' _):cl) | ordemTCF tcf' == ordemTCF tcf -> (Just c,cl)
       _ -> (Nothing,comps')

alteraFragmento f (URNLexML local doc versao forma fragmento) = URNLexML local doc versao forma (f fragmento)

selecionaFragmento :: TipoComponenteFragmento -> [Integer] -> URNLexML -> URNLexML
selecionaFragmento = selecionaFragmento' True

selecionaFragmento' :: Bool -> TipoComponenteFragmento -> [Integer] -> URNLexML -> URNLexML
selecionaFragmento' canReplace tcf nl = alteraFragmento f
  where
    f (Just (Fragmento comps)) = Just $ Fragmento $ before ++ [c] ++ after
      where (before,m,after) = splitFragmento tcf comps
            nc = CompFragmento tcf nl
            c = maybe nc (if canReplace then const nc else id) m
    f _ = Just $ Fragmento $ [CompFragmento tcf nl]

selecionaInciso n = selecionaFragmento TCF_Inciso n . selecionaFragmento' False TCF_Caput []
selecionaParagrafo = selecionaFragmento TCF_Paragrafo
selecionaCaput = selecionaFragmento TCF_Caput []
selecionaAlinea = selecionaFragmento TCF_Alinea
selecionaItem = selecionaFragmento TCF_Item
selecionaArtigo = selecionaFragmento TCF_Artigo

selecionaNorma = selecionaNormaAutoridade Nothing

selecionaNormaAutoridade mautoridade tipo numLei dia mes ano (URNLexML local (Documento autoridade  _ _) _ _ _) =
    URNLexML local (Documento (maybe autoridade id mautoridade) (TipoDocumento1 (STD1_Norma (TipoNorma (Nome tipo))) Nothing) (Descritor (TD_Datas (Datas (Left $ Data ano mes dia)) (Just $ ID_Ids [IdDocumento $ makeNormalID numLei])) [] Nothing) ) Nothing Nothing Nothing

selecionaNorma' = selecionaNormaAutoridade' Nothing

selecionaNormaAutoridade' mautoridade tipo numLei ano (URNLexML local (Documento autoridade  _ _) _ _ _) =
    URNLexML local (Documento (maybe autoridade id mautoridade)  (TipoDocumento1 (STD1_Norma (TipoNorma (Nome tipo))) Nothing) (Descritor (TD_Ano ano (ID_Ids [IdDocumento $ makeNormalID numLei])) [] Nothing) ) Nothing Nothing Nothing

selecionaLocal :: Local -> URNLexML -> URNLexML
selecionaLocal local (URNLexML _ doc vers form frag) = URNLexML local doc vers form frag 

selecionaNorma'' :: [String] -> [Integer] -> Int -> Maybe (Int,Int) -> Maybe ([String],[String]) -> Maybe [String] -> Maybe Autoridade -> 
                    URNLexML -> URNLexML
selecionaNorma'' tipo num ano mmd mmunicipio mestado mautoridade (URNLexML (Local Brasil local) (Documento autoridade  _ _) _ _ _) =
    URNLexML (Local Brasil local') (Documento autoridade' (TipoDocumento1 (STD1_Norma (TipoNorma (Nome tipo))) Nothing) (Descritor tipoDesc [] Nothing)) Nothing Nothing Nothing
  where
    (local',autoridade') = case mmunicipio of
        Just (m,e) -> (Just $ DLNormal (UnidadeFederacao $ Nome $ e) (Just $ Municipio $ Nome m ),
                          A_Convencionada $ seNaoDistrital e AC_Municipal) 
        Nothing -> case mestado of
                    Just e -> (Just $ DLNormal (UnidadeFederacao $ Nome $ e) Nothing, A_Convencionada $ seNaoDistrital e AC_Estadual)
                    Nothing -> case tipo of
                      ["resolucao"] -> (local, maybe autoridade id mautoridade)
                      _ -> case autoridade of
                              A_Normal [SJ_Instituicao (Instituicao (Nome [x,"federal"])) [] Nothing] -> 
                                    (local, maybe (A_Convencionada AC_Federal) id mautoridade)
                              _ -> (local, maybe autoridade id mautoridade) 
    tipoDesc = case mmd of
      Nothing -> TD_Ano ano idents
      Just (m,d) -> TD_Datas (Datas $ Left $ Data ano m d) (Just idents)
    
    idents = ID_Ids [IdDocumento $ makeNormalID num]
    seNaoDistrital ["distrito","federal"] _ = AC_Distrital
    seNaoDistrital _ a = a

selecionaLeiApelido nl (URNLexML local (Documento autoridade  _ _) _ _ _) =
        URNLexML local (Documento autoridade  (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["lei"]))) Nothing) (Descritor (TD_Apelido Nothing (ApelidoDocumento (Nome nl))) [] Nothing) ) Nothing Nothing Nothing

apelidoCLT = normaFederal ["decreto","lei"] [5452] 1 5 1943

apelidoRegimentoInternoSenado = 
    URNLexML (Local Brasil Nothing) (Documento autoridadeSenado (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["regimento","interno"]))) Nothing) (Descritor (TD_Datas (Datas (Left $ Data 1970 11 27)) (Just $ ID_Ids [IdDocumento $ makeNormalID [1970]])) [] Nothing) ) Nothing Nothing Nothing

apelidoRegimentoInternoCamara = 
    URNLexML (Local Brasil Nothing) (Documento autoridadeCamara (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["regimento","interno"]))) Nothing) (Descritor (TD_Datas (Datas (Left $ Data 1989 9 21)) (Just $ ID_Ids [IdDocumento $ makeNormalID [1989]])) [] Nothing) ) Nothing Nothing Nothing

apelidoRegimentoComumCongresso = 
    URNLexML (Local Brasil Nothing) (Documento autoridadeCongresso (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["regimento","interno"]))) Nothing) (Descritor (TD_Datas (Datas (Left $ Data 1970 8 11)) (Just $ ID_Ids [IdDocumento $ makeNormalID [1970]])) [] Nothing) ) Nothing Nothing Nothing

apelidoRegimentoInterno urn@(URNLexML (Local Brasil Nothing) (Documento aut _ _) _ _ _ ) =
  if aut == autoridadeCamara  then apelidoRegimentoInternoCamara 
      else apelidoRegimentoInternoSenado

apelidoRegulamentoAdministrativoSenado = 
    URNLexML (Local Brasil Nothing) (Documento autoridadeSenado (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["resolucao"]))) Nothing) (Descritor (TD_Datas (Datas (Left $ Data 1972 11 10)) (Just $ ID_Ids [IdDocumento $ makeNormalID [58]])) [] Nothing) ) Nothing Nothing Nothing

atoDisposicoesConstitucionaisTrans = 
    URNLexML (Local Brasil Nothing) 
             (Documento (A_Convencionada AC_Federal) 
                        (TipoDocumento1 (STD1_Norma (TipoNorma (Nome nome))) Nothing) 
                        (Descritor (TD_Datas (Datas (Left $ Data 1988 10 05)) 
                                             (Just $ ID_Ids [IdDocumento $ makeNormalID [1988]])) 
                                   [] Nothing) ) 
             Nothing Nothing Nothing
   where
      nome = ["ato","disposicoes","constitucionais","transitorias"]

makeNormalID :: [Integer] -> NormalID
makeNormalID nl = NormalID $ concat (intersperse "-" (map show nl))

autoridadeConstituicao (URNLexML local (Documento a@(A_Convencionada _) _ _) _ _ _) = (local, a)
autoridadeConstituicao (URNLexML local@(Local Brasil Nothing) _ _ _ _) = (local, A_Convencionada AC_Federal)
autoridadeConstituicao (URNLexML local@(Local Brasil (Just (DLNormal _ Nothing))) _ _ _ _) = (local, A_Convencionada AC_Estadual)
autoridadeConstituicao (URNLexML local@(Local Brasil (Just (DLNormal (UnidadeFederacao (Nome ["distrito", "federal"])) _))) _ _ _ _) = 
                                  (local, A_Convencionada AC_Distrital)
autoridadeConstituicao (URNLexML local@(Local Brasil (Just (DLNormal uf (Just _)))) _ _ _ _) = 
                                  (Local Brasil (Just (DLNormal uf Nothing)), A_Convencionada AC_Estadual)
autoridadeConstituicao (URNLexML local (Documento aut _ _) _ _ _) = (local,aut)

selecionaConstituicao _ = URNLexML (Local Brasil Nothing) (Documento (A_Convencionada AC_Federal) (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["constituicao"]))) Nothing) (Descritor (TD_Datas (Datas (Left (Data 1988 10 05))) (Just $ ID_Ids [IdDocumento $ NormalID "1988" ] )) [] Nothing)) Nothing Nothing Nothing

selecionaResolucao = selecionaNormaAutoridade' (Just autoridadeSenado) ["resolucao"]

autoridadeSenado = A_Normal [SJ_Instituicao (Instituicao $ Nome ["senado","federal"]) [] Nothing]

autoridadeCamara = A_Normal [SJ_Instituicao (Instituicao $ Nome ["camara","deputados"]) [] Nothing]

autoridadeCongresso = A_Normal [SJ_Instituicao (Instituicao $ Nome ["congresso","nacional"]) [] Nothing]

selecionaMunicipio :: [String] -> [String] -> URNLexML -> URNLexML
selecionaMunicipio nomeMunicipio nomeEstado (URNLexML local doc mversao mforma mfragmento) = 
      URNLexML (Local Brasil (Just (DLNormal (UnidadeFederacao $ Nome nomeEstado) (Just $ Municipio $ Nome nomeMunicipio)))) doc mversao mforma mfragmento


selecionaConstituicao' :: Maybe Int -> Maybe (Int,Int) -> Maybe [String] -> Maybe Data -> URNLexML -> URNLexML
selecionaConstituicao' mano mmd' mestado mvig (URNLexML (Local Brasil local) (Documento autoridade  _ _) _ _ _) =
    URNLexML (Local Brasil local') (Documento autoridade' (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["constituicao"]))) Nothing) (Descritor tipoDesc [] Nothing)) mversao Nothing Nothing
  where
    mversao = case mvig of
      Just d -> Just $ Versao (TV_VersaoVigenteEm d) Nothing
      _ -> Nothing
    (local',autoridade') = case mestado of
                    Just e -> (Just $ DLNormal (UnidadeFederacao $ Nome $ e) Nothing, A_Convencionada AC_Estadual)
                    Nothing -> (local, A_Convencionada AC_Federal)
    tipoDesc = case mano of
      Nothing -> TD_Apelido Nothing (ApelidoDocumento $ Nome $ ["constituicao"])
      Just ano -> let idents = ID_Ids [IdDocumento $ NormalID $ show ano]
                      in case mmd' of
                          Nothing -> TD_Ano ano idents
                          Just (m,d) -> TD_Datas (Datas $ Left $ Data ano m d) (Just idents)
