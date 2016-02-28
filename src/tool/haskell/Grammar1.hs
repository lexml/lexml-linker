module Grammar1 where

import Prelude hiding (lex)
import GrammarGenerator 
import Control.Monad.Writer


l s = lex $ "'" ++ s ++ "'"
lexSeq = PE_Concat . map l
lexAlt = PE_Choice . map l


data ComponenteParseInfo = CPI {
      codigo :: String,
      peNomesCompPlural :: [ProdExpr],      
      artigosPlural :: [String],
      peNomesCompSingular :: [ProdExpr],
      artigosSingular :: [String],
      numerosComp :: [ProdExpr],
      peNumerosEspecificos :: [ProdExpr],
      subComponente :: Maybe ComponenteParseInfo,
      preposicoes :: [String]
  } deriving (Eq,Ord,Show)

peArtigosPlural = PE_Choice . map l . artigosPlural
prepArtigoPlural = map ("d" ++) . artigosPlural
pePrepArtigoPlural = PE_Choice . map l . prepArtigoPlural
prepArtigoSing = map ("d" ++) . artigosSingular
pePrepArtigoSing = PE_Choice . map l . prepArtigoSing

nt1 n p = PN n [PN p []]

pn_compDirComNome' comp =  nt1 "compDirNome'" (codigo comp)
compDirComNome' =  PE_NonTerminal . pn_compDirComNome'
pn_listaDe alpha = PN "listaDe" [alpha]
listaDe = PE_NonTerminal . pn_listaDe 
pn_compDirComNome'_1 comp = nt1 "compDirComNome'_1" (codigo comp)
compDirComNome'_1 = PE_NonTerminal . pn_compDirComNome'_1
pn_compDirPlural c = nt1 "compDirPlural" (codigo c)
compDirPlural = PE_NonTerminal . pn_compDirPlural 
pn_compDirSingComNome c = nt1 "compDirSingComNome" (codigo c)
compDirSingComNome = PE_NonTerminal . pn_compDirSingComNome
pn_compDirNomeOpt' c = nt1 "compDirNomeOpt'" (codigo c)
compDirNomeOpt' = PE_NonTerminal . pn_compDirNomeOpt'
pn_compDirSingPrelim c = nt1 "compDirSingPrelim" (codigo c)
compDirSingPrelim = PE_NonTerminal . pn_compDirSingPrelim
pn_nomes alpha = PN "nomes" [alpha]
nomes = PE_NonTerminal . pn_nomes
pn_componente c = nt1 "componente" (codigo c)
componente = PE_NonTerminal . pn_componente
pn_compPlural2 c = nt1 "compPlural2" (codigo c)
compPlural2 = PE_NonTerminal . pn_compPlural2
pn_compPlural2_a c = nt1 "compPlural2_a" (codigo c)
compPlural2_a = PE_NonTerminal . pn_compPlural2_a 
pn_compPlural2_b c = nt1 "compPlural2_b" (codigo c)
compPlural2_b = PE_NonTerminal . pn_compPlural2_b 
pn_compPlural2_c c = nt1 "compPlural2_c" (codigo c)
compPlural2_c = PE_NonTerminal . pn_compPlural2_c 
pn_compPlural1 c = nt1 "compPlural1" (codigo c)
compPlural1 = PE_NonTerminal . pn_compPlural1
pn_compPlural1_a c n = PN "compPlural1_a" [PN (codigo c) [], PN n []]
compPlural1_a = (PE_NonTerminal .) . pn_compPlural1_a
pn_compSing c = nt1 "compSing" (codigo c)
compSing = PE_NonTerminal . pn_compSing
pn_compSing2 c = nt1 "compSing2" (codigo c)
compSing2 = PE_NonTerminal . pn_compSing2
pn_compSing1  c = nt1 "compSing1" (codigo c)
compSing1 = PE_NonTerminal . pn_compSing1
pn_listaDe' pn sep = PN "listaDe'" [pn,PN (case sep of ";" -> "semi" ; _ -> "comma") []]
listaDe' = (PE_NonTerminal .) . pn_listaDe'
pn_numeros t = PN "numeros" [PN t []]
numeros = PE_NonTerminal . pn_numeros
pn_variosNumeros' t = PN "variosNumeros'" [PN t []]
variosNumeros' = PE_NonTerminal . pn_variosNumeros'
pn_nomesCompPlural c = nt1 "nomesCompPlural" (codigo c)
nomesCompPlural = PE_NonTerminal . pn_nomesCompPlural
pn_nomesCompSingular c = nt1 "nomesCompSingular" (codigo c)
nomesCompSingular = PE_NonTerminal . pn_nomesCompSingular
pn_numerosEspecificos c = nt1 "numerosEspecificos" (codigo c)
numerosEspecificos = PE_NonTerminal . pn_numerosEspecificos

cpiBase = CPI {
      codigo = ""
    , peNomesCompPlural = []
    , artigosPlural = []
    , peNomesCompSingular = []
    , artigosSingular = []
    , numerosComp = []
    , peNumerosEspecificos = []
    , subComponente = Nothing
    , preposicoes = []
}

cpiFeminino = cpiBase {
      artigosPlural = ["as"]
    , artigosSingular = ["a"]
    , preposicoes = ["da"]
}

cpiMasculino = cpiBase {
      artigosPlural = ["os"]
    , artigosSingular = ["o"]
    , preposicoes = ["do"]
}


compItem ::  ComponenteParseInfo
compItem = cpiMasculino {
      peNomesCompPlural = [l "itens"]
    , peNomesCompSingular = [l "item"]
    , numerosComp = [numeros "numeroArabico"]
    , codigo = "item"
  }

compAlinea ::  ComponenteParseInfo
compAlinea = cpiFeminino {
      peNomesCompPlural = [l "alineas", l "letras"]
    , peNomesCompSingular = [l "alinea",l "letra"]
    , numerosComp = [numeros "numeroAlfabeto",numeros "numeroArabico", numeros "numeroRomano"]
    , peNumerosEspecificos = [numeros "numeroAlfabeto'"]
    , subComponente = Just compItem
    , codigo = "alinea"
  }


compInciso ::  ComponenteParseInfo
compInciso = cpiMasculino {
      peNomesCompPlural = [l "incisos", l "incs" *** opt (l ".")]
    , peNomesCompSingular = [l "inciso", l "inc" *** opt (l ".")]
    , numerosComp = [numeros "numeroArabico", numeros "numeroRomano"]
    , peNumerosEspecificos = [numeros "numeroRomano"]
    , subComponente = Just compAlinea
    , codigo = "inciso"
  }

compParagrafo ::  ComponenteParseInfo
compParagrafo = cpiMasculino {
      peNomesCompPlural = [l "paragrafos", l "pars" *** opt (l "."), lex "SYM_PARAGRAFOS", lex "SYM_PARAGRAFO" *** lex "SYM_PARAGRAFO"]
    , peNomesCompSingular = [nt0 "caput", l "paragrafo", l "par" *** opt (l "."), lex "SYM_PARAGRAFO"]
    , numerosComp = [variosNumeros' "numeroOrdinal", variosNumeros' "numeroArabico"]
    , peNumerosEspecificos = [variosNumeros' "numeroOrdinal", variosNumeros' "numeroArabicoMaiorQue10"]
    , subComponente = Just compInciso
    , codigo = "paragrafo"
  }  

compArtigo ::  ComponenteParseInfo
compArtigo = cpiMasculino {
      peNomesCompPlural = [l "artigos", l "arts" *** opt (l ".")]
    , peNomesCompSingular = [l "artigo", l "art" *** opt (l ".")]
    , numerosComp = [variosNumeros' "numeroOrdinal", variosNumeros' "numeroArabico"]
    , peNumerosEspecificos = [numeros "numeroOrdinal",numeros "numeroArabicoMaiorQue10"]
    , subComponente = Just compParagrafo
    , codigo = "artigo"
  }

allComps = [compArtigo,compParagrafo,compInciso,compAlinea,compItem]
forAllComps :: (ComponenteParseInfo  -> Prod) -> GrammarWriter ()
forAllComps f = mapM_ (\c -> tell [f c]) allComps

linkerGrammar = grammar $ do
  "norma" #= ( compDirComNome' compArtigo |||
               componente compArtigo ) *** nt0 "norma'"
  "norma'" #= opt ( opt (l ",") *** opt (l "todos" ||| l "todas" ||| l "ambos" ||| l "ambas") *** (l "de" ||| l "do" ||| l "da" )) *** (nt0 "constituicao1988" ||| nt0 "apelidos" ||| nt0 "normaExtenso" )
  "constituicao1988" #= l "constituicao" *** opt (l "federal")
  "apelidos" #= lexSeq ["regimento","interno","do","senado"] *** opt (l "federal") |||
                lexSeq ["regulamento","administrativo","do","senado","federal"] |||
                lexSeq ["ato","das","disposicoes","constitucionais","transitorias"]
  "normaExtenso" #= nt0 "tipoNorma" *** opt (nt0 "qualificadores'")
  "tipoNorma" #= nt0 "tnLei" ||| nt0 "tnResolucao"  ||| nt0 "tnDecreto" ||| 
                 nt0 "tnEmendaConstitucional" ||| nt0 "tnMedidaProvisoria" ||| 
                 nt0 "tnCLT" ||| nt0 "tnCF"
  "tnLei" #= ( l "lei" ||| l "leis" ) ***
             opt ( lexAlt ["complementar","complementares","delegada","delegadas"] ) ***
             opt ( lexAlt ["federal","federais","estadual","estaduais","municipal",
                           "municipais","distrital","distritais"] )
  "tnResolucao" #= l "resolucao" ||| l "resolucoes"
  "tnDecreto" #= (l "decreto" ||| l "decretos" ) *** opt (l "lei") ***
             opt ( lexAlt ["federal","federais","estadual","estaduais","municipal",
                           "municipais","distrital","distritais"] )
  "tnEmendaConstitucional" #= l "emenda" *** l "constitucional"
  "tnMedidaProvisoria" #= ( l "medida" ||| l "medidas" ) ***
                          ( l "provisoria" ||| l "provisorias" )
  "tnClt" #= l "clt" ||| lexSeq ["consolidacao","das","leis","do","trabalho"]
  "tnCF" #= l "cf" *** l "/" *** lex "NUMERO"
  "qualificadores" #= many1 (nt0 "qualificador") ||| l "-" *** many (lex "NOT('e')")
  "qualificador" #= ( opt (l "," ||| l ";") *** 
                     ( nt0 "qualNumero" ||| nt0 "qualData" |||
                       nt0 "qualData2" ||| nt0 "qualMunicipio" |||
                       nt0 "qualEstado" ) ) ||| nt0 "qualAutoridade"
  "qualAutoridade" #= nt0 "qualAutoridadeHifen" ||| nt0 "qualAutoridadeExtenso"
  "qualAutoridadeHifen" #= l "-" *** l "cn"
  "qualAutoridadeExtenso" #= lexSeq ["do","conselho","deliberativo","do",
                                     "fundo","de","amparo","ao","trabalhador"] |||
                             lexSeq ["do","senado","federal"]
  "qualEstado" #= opt ( opt (l "do") *** l "estado" *** (l "de" ||| l "do" ||| l "da") ) *** nt0 "estado"
  "estado" #= l "goias" ||| l "minas" *** l "gerais"
  "qualMunicipio" #= l "municipio" *** ( l "de" ||| l "do" ||| l "da" ) *** nt0 "municipio"
  "municipio" #= l "goiania" ||| l "belo" *** l "horizonte"
  "qualData" #= opt (lexAlt ["ambos","ambas","todos","todas"]) *** opt (l "de") *** (nt0 "data" ||| nt0 "ano4")
  "qualData2" #= l "/" *** (nt0 "ano4" ||| nt0 "ano2")
  "qualNumero" #= opt (l "e") *** opt (nt0 "abrevNumero") *** nt0 "numerosArabico_nalfa"
  "numerosArabico_nalfa" #= nt0 "numeroArabico" *** many (l "-" *** (lex "NUMERO" ||| nt0 "numAlfabeto"))

  forAllComps $ \c -> pn_nomesCompSingular c ::= PE_Choice (peNomesCompSingular c)
  forAllComps $ \c -> pn_nomesCompPlural c ::= PE_Choice (peNomesCompPlural c)
  forAllComps $ \c -> pn_numerosEspecificos c ::= PE_Choice (peNumerosEspecificos c)

  forAllComps $ \c -> pn_compDirComNome' c ::= listaDe (pn_compDirComNome'_1 c)
  forAllComps $ \c -> pn_compDirComNome'_1 c ::= (
      if c == compParagrafo then 
        compDirPlural c ||| compDirSingComNome c ||| compDirComNome' compInciso 
      else 
        compDirPlural c ||| compDirSingComNome c
    )
  forAllComps $ \c -> 
    pn_compDirPlural c ::= 
        opt (peArtigosPlural c ||| l "nos" ||| l "nas") ***
        opt (l "termos" *** pePrepArtigoPlural c) ***
        nomes (pn_nomesCompPlural c) ***
        compDirNomeOpt' c
                          
  
