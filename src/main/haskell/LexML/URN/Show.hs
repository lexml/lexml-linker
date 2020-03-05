{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RelaxedPolyRec #-}
{-# LANGUAGE FlexibleInstances #-}

module LexML.URN.Show where

import LexML.URN.Types
import Text.Printf

class URNShow a where
  urnShow :: a -> String

instance URNShow Integer where
  urnShow = show

instance URNShow Int where
  urnShow = show

instance URNShow String where
  urnShow = id

instance URNShow URNLexML where
  urnShow (URNLexML local doc mversao mforma mfragmento) =
    "urn:lex:" ++ urnShow local ++ ":" ++ urnShow doc ++ prefixJust "@" mversao
    ++ prefixJust "~" mforma ++ prefixJust "!" mfragmento

instance URNShow Local where
  urnShow (Local pais mdetalhamento) = urnShow pais ++ prefixJust ";" mdetalhamento

instance URNShow Data where
  urnShow (Data ano mes dia) = printf "%04u-%02u-%02u" ano mes dia

instance URNShow Datas where
  urnShow (Datas (Left d)) = urnShow d
  urnShow (Datas (Right (d1,d2))) = "[" ++ urnShow d1 ++ "," ++ urnShow d2 ++ "]"

instance URNShow Pais where
  urnShow Brasil = "br"

instance URNShow DetalhamentoLocal where
  urnShow (DLNormal uf mmunicipio) = urnShow uf ++ prefixJust ";" mmunicipio
  urnShow (DLJudiciario muf localJudiciario) = suffixJust ";" muf ++ urnShow localJudiciario

instance URNShow LocalJudiciario where
  urnShow (LocalJudiciario ramoJustica detalhes) =
    urnShow ramoJustica ++ prefixAll ";" detalhes

instance URNShow RamoJustica where
  urnShow RJ_Federal = "federal"
  urnShow RJ_Trabalho = "trabalho"
  urnShow RJ_Eleitoral = "eleitoral"
  urnShow RJ_Militar = "militar"
  urnShow RJ_Estadual = "estadual"

instance URNShow UnidadeFederacao where
  urnShow (UnidadeFederacao nome) = urnShow nome

instance URNShow DetalheRamo where
  urnShow (DR_Regiao n) = "regiao." ++ urnShow n
  urnShow (DR_Zona n) = "zona." ++ urnShow n
  urnShow DR_SecaoJudiciaria = "secao.judiciaria"
  urnShow DR_Comarca = "comarca"
  urnShow (DR_UnidadesFederacao unidades) = showBetween "," unidades
  urnShow (DR_Municipio municipio) = urnShow municipio

instance URNShow Municipio where
  urnShow (Municipio nome) = urnShow nome

instance URNShow Nome where
  urnShow (Nome componentes) = between "." componentes

instance URNShow Documento where
  urnShow (Documento autoridade tipoDocumento descritor) =
    urnShow autoridade ++ ":" ++ urnShow tipoDocumento ++ ":" ++ urnShow descritor

instance URNShow Autoridade where
  urnShow (A_Normal sujeitos) = showBetween "," sujeitos
  urnShow (A_Convencionada ac) = urnShow ac

instance URNShow AutoridadeConvencionada where
  urnShow AC_Federal = "federal"
  urnShow AC_Estadual = "estadual"
  urnShow AC_Municipal = "municipal"
  urnShow AC_Distrital = "distrital"

instance URNShow Sujeito where
  urnShow (SJ_Instituicao instituicao orgaos mfuncao) =
    urnShow instituicao ++ prefixAll ";" orgaos ++ prefixJust ";" mfuncao
  urnShow (SJ_Cargo cargo) = urnShow cargo

instance URNShow Instituicao where
  urnShow (Instituicao nome) = urnShow nome

instance URNShow Orgao where
  urnShow (Orgao nome) = urnShow nome

instance URNShow Funcao where
  urnShow (Funcao nome) = urnShow nome

instance URNShow Cargo where
  urnShow (Cargo nome) = urnShow nome

instance URNShow TipoDocumento where
  urnShow (TipoDocumento1 subTipo mnomeSubtipoSequenciador) =
      urnShow subTipo ++ prefixJust ";" mnomeSubtipoSequenciador
  urnShow (TipoDocumento2 nomePeriodicoOficial mnomeSecaoPeriodicoOficial mdetalheExtraSuplemento) =
      "publicacao.oficial;" ++ urnShow nomePeriodicoOficial ++ prefixJust ";" mnomeSecaoPeriodicoOficial ++
      prefixJust ";" mdetalheExtraSuplemento

instance URNShow SubTipoDocumento where
  urnShow (STD1_Norma tipoNorma) = urnShow tipoNorma
  urnShow (STD1_Jurisprudencia jurisprudencia) = urnShow jurisprudencia
  urnShow (STD1_ProjetoNorma projetoNorma) = urnShow projetoNorma

instance URNShow TipoProjetoNorma where
  urnShow (TipoProjetoNorma nome) = urnShow nome

instance URNShow TipoNorma where
  urnShow (TipoNorma nome) = urnShow nome

instance URNShow TipoJurisprudencia where
  urnShow (TipoJurisprudencia nome) = urnShow nome

instance URNShow NomeSubtipoSequenciador where
  urnShow (NomeSubtipoSequenciador nome) = urnShow nome

instance URNShow NomePeriodicoOficial where
  urnShow (NomePeriodicoOficial nome) = urnShow nome

instance URNShow NomeSecaoPeriodicoOficial where
  urnShow (NomeSecaoPeriodicoOficial nome) = urnShow nome

instance URNShow DetalheExtraSuplemento where
  urnShow (DetalheExtraSuplemento tipoDetalhe mnumero) =
    urnShow tipoDetalhe ++ prefixJust "." mnumero

instance URNShow TipoDetalheExtraSuplemento where
  urnShow TDES_EdicaoExtra = "edicao.extra"
  urnShow TDES_Suplemento = "suplemento"

instance URNShow Descritor where
  urnShow (Descritor tipoDescritor comps mseqRetificacao) =
    urnShow tipoDescritor ++ prefixAll ";" comps ++ prefixJust ";retificacao." mseqRetificacao

instance URNShow TipoDescritor where
  urnShow (TD_Datas datas midentificadores) = urnShow datas ++ prefixJust ";" midentificadores
  urnShow (TD_Ano ano identificadores) = urnShow ano ++ ";" ++ urnShow identificadores
  urnShow (TD_Apelido mdataOuAno apelido) = suffixJust ";" mdataOuAno ++ urnShow apelido

instance URNShow DatasOuAno where
  urnShow (DatasOuAno e) = either urnShow urnShow e

instance URNShow ComponenteDescritor where
  urnShow (ComponenteDescritor idcomp mtitulo) =
    urnShow idcomp ++ prefixJust "," mtitulo

instance URNShow ApelidoDocumento where
  urnShow (ApelidoDocumento nome) = urnShow nome

instance URNShow IdComponente where
  urnShow (IdComponente nome) = urnShow nome

instance URNShow TituloComponente where
  urnShow (TituloComponente nome) = urnShow nome

instance URNShow Identificadores where
  urnShow (ID_Ids ids) = showBetween "," ids
  urnShow (ID_NumeroLex n) = "lex-" ++ urnShow n
  urnShow (ID_NumeroSeq n) = urnShow n

instance URNShow NumeroSeq where
  urnShow (NumeroSeq sigla n) = "seq-" ++ urnShow sigla ++ "-" ++ urnShow n

instance URNShow SiglaOrgao where
  urnShow (SiglaOrgao n) = urnShow n

instance URNShow IdDocumento where
  urnShow (IdDocumento idd) = urnShow idd

instance URNShow NormalID where
  urnShow (NormalID n) = n


instance URNShow Fragmento where
  urnShow (Fragmento comps) = showBetween "_" comps

instance URNShow CompFragmento where
  urnShow (CompFragmento tipoComponente UI_Unico) =
    urnShow tipoComponente ++ "1u"
  urnShow (CompFragmento tipoComponente (UI_Indices indices)) =
    urnShow tipoComponente ++ showBetween "-" indices

instance URNShow TipoComponenteFragmento where
  urnShow TCF_Parte                  = "prt"
  urnShow TCF_Livro                  = "liv"
  urnShow TCF_Titulo                 = "tit"
  urnShow TCF_Capitulo               = "cap"
  urnShow TCF_Secao                  = "sec"
  urnShow TCF_SubSecao               = "sub"
  urnShow TCF_AgrupamentoHierarquico = "agh"
  urnShow TCF_Artigo                 = "art"
  urnShow TCF_Caput                  = "cpt"
  urnShow TCF_Paragrafo              = "par"
  urnShow TCF_Inciso                 = "inc"
  urnShow TCF_Alinea                 = "ali"
  urnShow TCF_Item                   = "ite"
  urnShow TCF_DispositivoGenerico    = "dpg"
  urnShow TCF_Aspas                  = "asp"

instance URNShow Versao where
  urnShow (Versao tipoVersao mdetalhe) =
    urnShow tipoVersao ++ prefixJust ";" mdetalhe

instance URNShow TipoVersao where
  urnShow (TV_Datas datas) = urnShow datas
  urnShow TV_VersaoOriginal = "versao.original"
  urnShow TV_InicioVigencia = "inicio.vigencia"
  urnShow TV_MultiVigente = "multivigente"
  urnShow (TV_VersaoVigenteEm d) = "versao.vigente.em;" ++ urnShow d
  urnShow (TV_VersaoEficazEm d) = "versao.eficaz.em;" ++ urnShow d
  urnShow (TV_VersaoConsultadaEm d) = "versao.consultada.em;" ++ urnShow d

instance URNShow DetalheVersao where
  urnShow (DetalheVersao evento mvisao) = urnShow evento ++ prefixJust ";" mvisao

instance URNShow Evento where
  urnShow (Evento nome) = urnShow nome

instance URNShow Visao where
  urnShow (Visao datas) = urnShow datas

instance URNShow Forma where
  urnShow (Forma tipoForma linguas) =
    urnShow tipoForma ++ (if null linguas then "" else ";") ++ showBetween "," linguas

instance URNShow TipoForma where
  urnShow (TipoForma nome) = urnShow nome

instance URNShow Lingua where
  urnShow (Lingua linguaCodigo mlinguaScript mlinguaRegiao) =
    urnShow linguaCodigo ++ prefixJust "-" mlinguaScript ++ prefixJust "-" mlinguaRegiao

instance URNShow LinguaCodigo where
  urnShow (LinguaCodigo nome) = nome

instance URNShow LinguaScript where
  urnShow (LinguaScript nome) = nome

instance URNShow LinguaRegiao where
  urnShow (LinguaRegiao nome) = nome

prefixJust :: URNShow a => String -> Maybe a -> String
prefixJust _      Nothing = ""
prefixJust prefix (Just x) = prefix ++ urnShow x

suffixJust :: URNShow a => String -> Maybe a -> String
suffixJust _ Nothing = ""
suffixJust suffix (Just x) = urnShow x ++ suffix

prefixAll :: URNShow a => String -> [a] -> String
prefixAll prefix l = concat [ prefix ++ urnShow el | el <- l ]

between :: String -> [String] -> String
between sep [] = ""
between sep [x] = x
between sep (x:xs) = x ++ sep ++ showBetween sep xs

showBetween :: URNShow a => String -> [a] -> String
showBetween sep = between sep . map urnShow

