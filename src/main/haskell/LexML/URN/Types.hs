{-# LANGUAGE DeriveDataTypeable #-}
module LexML.URN.Types where

import Data.Typeable
import qualified Data.Generics as G

data URNLexML = URNLexML Local Documento (Maybe Versao) (Maybe Forma) (Maybe Fragmento) deriving (Eq,Ord,Typeable,Show,Read,G.Data)

type Ano = Int

type Mes = Int

type Dia = Int

data Local = Local Pais (Maybe DetalhamentoLocal) deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data Data = Data Ano Mes Dia deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype Datas = Datas (Either Data (Data,Data)) deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data Pais = Brasil deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data DetalhamentoLocal = 
    DLNormal UnidadeFederacao (Maybe Municipio) 
  | DLJudiciario (Maybe UnidadeFederacao) LocalJudiciario
      deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data LocalJudiciario = LocalJudiciario RamoJustica [DetalheRamo] deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data RamoJustica = RJ_Federal | RJ_Trabalho | RJ_Eleitoral | RJ_Militar | RJ_Estadual deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype UnidadeFederacao = UnidadeFederacao Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data DetalheRamo = 
    DR_Regiao Integer
  | DR_Zona Integer
  | DR_SecaoJudiciaria
  | DR_Comarca
  | DR_UnidadesFederacao [UnidadeFederacao]
  | DR_Municipio Municipio
      deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype Municipio = Municipio Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype Nome = Nome [String] deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data Documento = Documento Autoridade TipoDocumento Descritor deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data Autoridade = A_Normal [Sujeito] | A_Convencionada AutoridadeConvencionada deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data AutoridadeConvencionada = AC_Federal | AC_Estadual | AC_Municipal | AC_Distrital deriving (Eq,Ord, Typeable, Show,Read,G.Data)

data Sujeito = 
    SJ_Instituicao Instituicao [Orgao] (Maybe Funcao)
  | SJ_Cargo Cargo
      deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype Instituicao = Instituicao Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype Orgao = Orgao Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype Funcao = Funcao Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype Cargo = Cargo Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data TipoDocumento =  
    TipoDocumento1 SubTipoDocumento (Maybe NomeSubtipoSequenciador) 
  | TipoDocumento2 NomePeriodicoOficial (Maybe NomeSecaoPeriodicoOficial) (Maybe DetalheExtraSuplemento)
      deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data SubTipoDocumento = 
    STD1_Norma TipoNorma
  | STD1_Jurisprudencia TipoJurisprudencia
  | STD1_ProjetoNorma TipoProjetoNorma
    deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype TipoProjetoNorma = TipoProjetoNorma Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype TipoNorma = TipoNorma Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype TipoJurisprudencia = TipoJurisprudencia Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype NomeSubtipoSequenciador = NomeSubtipoSequenciador Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype NomePeriodicoOficial = NomePeriodicoOficial Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype NomeSecaoPeriodicoOficial = NomeSecaoPeriodicoOficial Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data DetalheExtraSuplemento = DetalheExtraSuplemento TipoDetalheExtraSuplemento (Maybe Integer) deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data TipoDetalheExtraSuplemento = TDES_EdicaoExtra | TDES_Suplemento deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data Descritor = Descritor TipoDescritor [ComponenteDescritor] (Maybe SeqRetificacao) deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data TipoDescritor =
      TD_Datas Datas (Maybe Identificadores)
    | TD_Ano   Ano   Identificadores
    | TD_Apelido (Maybe DatasOuAno) ApelidoDocumento
      deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype DatasOuAno = DatasOuAno (Either Datas Ano) deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data ComponenteDescritor = ComponenteDescritor IdComponente (Maybe TituloComponente) deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype ApelidoDocumento = ApelidoDocumento Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype IdComponente = IdComponente Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype TituloComponente = TituloComponente Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

type SeqRetificacao = Integer

data Identificadores =
    ID_Ids [IdDocumento]
  | ID_NumeroLex NumeroLex
  | ID_NumeroSeq NumeroSeq
    deriving (Eq,Ord,Typeable,Show,Read,G.Data)

type NumeroLex = Integer

data NumeroSeq = NumeroSeq SiglaOrgao Integer deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype SiglaOrgao = SiglaOrgao Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype IdDocumento = IdDocumento NormalID deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype NormalID = NormalID String deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype Fragmento = Fragmento [CompFragmento] deriving (Eq,Ord,Typeable,Show,Read,G.Data)

type IndiceFragmento = Integer

data CompFragmento = CompFragmento TipoComponenteFragmento [IndiceFragmento] deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data TipoComponenteFragmento = 
    TCF_Parte     | TCF_Livro       | TCF_Titulo 
  | TCF_Capitulo  | TCF_Secao       | TCF_SubSecao 
  | TCF_AgrupamentoHierarquico      | TCF_Artigo 
  | TCF_Caput     | TCF_Paragrafo   | TCF_Inciso 
  | TCF_Alinea    | TCF_Item        | TCF_DispositivoGenerico
  | TCF_Aspas
      deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data Versao = Versao TipoVersao (Maybe DetalheVersao) deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data TipoVersao =
    TV_Datas Datas
  | TV_VersaoOriginal
  | TV_InicioVigencia
  | TV_MultiVigente
  | TV_VersaoVigenteEm Data
  | TV_VersaoEficazEm Data
  | TV_VersaoConsultadaEm Data
    deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data DetalheVersao = DetalheVersao Evento (Maybe Visao) deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype Evento = Evento Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype Visao = Visao Datas deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data Forma = Forma TipoForma [Lingua] deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype TipoForma = TipoForma Nome deriving (Eq,Ord,Typeable,Show,Read,G.Data)

data Lingua = Lingua LinguaCodigo (Maybe LinguaScript) (Maybe LinguaRegiao) deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype LinguaCodigo = LinguaCodigo String deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype LinguaScript = LinguaScript String deriving (Eq,Ord,Typeable,Show,Read,G.Data)

newtype LinguaRegiao = LinguaRegiao String deriving (Eq,Ord,Typeable,Show,Read,G.Data)

ex = URNLexML (Local Brasil (Just $ DLNormal (UnidadeFederacao $ Nome ["distrito","federal"]) (Just $ Municipio $ Nome ["brasilia"]))) 
              (Documento (A_Normal [
                            SJ_Instituicao (Instituicao $ Nome ["supremo","tribunal","federal"]) 
                                           [  Orgao $ Nome ["secretaria","tecnologia"], 
                                              Orgao $ Nome ["secretaria","judiciaria"] ] 
                                           (Just $ Funcao $ Nome ["secretario"]),
                            SJ_Cargo (Cargo $ Nome ["presidente","republica"])]) 
                         (TipoDocumento1 (STD1_Norma (TipoNorma $ Nome ["portaria"])) (Just $ NomeSubtipoSequenciador $ Nome ["abcd","12131"])) 
                         (Descritor (TD_Ano 2009 (ID_NumeroLex 4321)) [ComponenteDescritor (IdComponente $ Nome ["id","componente"]) (Just $ TituloComponente $ Nome ["titulo","componente"])] (Just $ 1234)))
              (Just $ Versao TV_InicioVigencia Nothing)
              (Just $ Forma (TipoForma $ Nome ["pdf"]) [])
              Nothing

