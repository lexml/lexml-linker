-- vim: tabstop=2 expandtab
module Grammar1 where

import Prelude hiding (lex)
import GrammarGenerator 
import Control.Monad.Writer
import qualified Data.Set as S
import ANTLR4Generator


l = PE_Lex . L_Literal  
ltype = PE_Lex . L_TokenType
lnot = PE_Lex . L_Not 

lexSeq = PE_Concat . map l
lexAlt = PE_Choice . map l


data ComponenteParseInfo = CPI {
      codigo :: String,
      pesNomesCompPlural :: [ProdExpr],      
      artigosPlural :: [String],
      pesNomesCompSingularDef   :: [ProdExpr],
      pesNomesCompSingularIndef :: [ProdExpr],
      artigosSingular :: [String],
      pesNumerosComp :: [ProdExpr],
      pesNumerosEspecificos :: [ProdExpr],
      subComponente :: Maybe ComponenteParseInfo,
      preposicoes :: [String]
  } deriving (Eq,Ord,Show)

peArtigosPlural = PE_Choice . map l . artigosPlural
prepArtigoPlural = map ("d" ++) . artigosPlural
pePrepArtigoPlural = PE_Choice . map l . prepArtigoPlural
peArtigosSingular = PE_Choice . map l . artigosSingular
prepArtigoSing = map ("d" ++) . artigosSingular
pePrepArtigoSing = PE_Choice . map l . prepArtigoSing
pePreposicoes = PE_Choice . map l . preposicoes
nomesCompSingularDef = PE_Choice . pesNomesCompSingularDef
nomesCompSingularIndef = PE_Choice . pesNomesCompSingularIndef
numerosComp = PE_Choice . pesNumerosComp

nt1 n p = PN n [PN p []]

pn_compDirComNome' comp =  nt1 "compDirComNome'" (codigo comp)
compDirComNome' =  PE_NonTerminal . pn_compDirComNome'
pn_listaDe alpha = PN "listaDe" [alpha]
listaDe = PE_NonTerminal . pn_listaDe 
pn_compDirComNome'_1 comp = nt1 "compDirComNome'_1" (codigo comp)
compDirComNome'_1 = PE_NonTerminal . pn_compDirComNome'_1
pn_compDirPlural c = nt1 "compDirPlural" (codigo c)
compDirPlural = PE_NonTerminal . pn_compDirPlural 
pn_compDirSingComNome c = nt1 "compDirSingComNome" (codigo c)
compDirSingComNome = PE_NonTerminal . pn_compDirSingComNome
pn_compDirSingComNome_1 c = nt1 "compDirSingComNome_1" (codigo c)
compDirSingComNome_1 = PE_NonTerminal . pn_compDirSingComNome_1
pn_compDirNomeOpt' c = nt1 "compDirNomeOpt'" (codigo c)
compDirNomeOpt' = PE_NonTerminal . pn_compDirNomeOpt'
pn_compDirSingPrelim c = nt1 "compDirSingPrelim" (codigo c)
compDirSingPrelim = PE_NonTerminal . pn_compDirSingPrelim
pn_compDirSingNomeOpt c = nt1 "compDirSingNomeOpt" (codigo c)
compDirSingNomeOpt = PE_NonTerminal . pn_compDirSingNomeOpt
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
pn_nomesCompPlural c = nt1 "nomesCompPlural" (codigo c)
nomesCompPlural = PE_NonTerminal . pn_nomesCompPlural
pn_nomesCompSingular c = nt1 "nomesCompSingular" (codigo c)
nomesCompSingular = PE_NonTerminal . pn_nomesCompSingular
pn_numerosEspecificos c = nt1 "numerosEspecificos" (codigo c)
numerosEspecificos = PE_NonTerminal . pn_numerosEspecificos
pn_preposicao c = nt1 "preposicao" (codigo c)
preposicao = PE_NonTerminal . pn_preposicao
pn_compDirNomeOpt'_1 c = nt1 "compDirNomeOpt'_1" (codigo c)
compDirNomeOpt'_1 = PE_NonTerminal . pn_compDirNomeOpt'_1

numeros tipoNumero = (nt0 tipoNumero) *** many (l "-" *** ltype "PALAVRA")
variosNumeros tipoNumero = (nt0 tipoNumero) *** many (l "-" *** (nt0 "numero" ||| nt0 "numeroAlfabeto"))

cpiBase = CPI {
      codigo = ""
    , pesNomesCompPlural = []
    , artigosPlural = []    
    , pesNomesCompSingularDef = []
    , pesNomesCompSingularIndef = []
    , artigosSingular = []
    , pesNumerosComp = []
    , pesNumerosEspecificos = []
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
      pesNomesCompPlural = [l "itens"]
    , pesNomesCompSingularIndef = [l "item"]
    , pesNumerosComp = [numeros "numeroArabico"]
    , codigo = "item"
  }

compAlinea ::  ComponenteParseInfo
compAlinea = cpiFeminino {
      pesNomesCompPlural = [l "alineas", l "letras"]
    , pesNomesCompSingularIndef = [l "alinea",l "letra"]
    , pesNumerosComp = [numeros "numeroAlfabeto",numeros "numeroArabico", numeros "numeroRomano"]
    , pesNumerosEspecificos = [numeros "numeroAlfabeto_"]
    , subComponente = Just compItem
    , codigo = "alinea"
  }


compInciso ::  ComponenteParseInfo
compInciso = cpiMasculino {
      pesNomesCompPlural = [l "incisos", l "incs" *** opt (l ".")]
    , pesNomesCompSingularIndef = [l "inciso", l "inc" *** opt (l ".")]
    , pesNumerosComp = [numeros "numeroArabico", numeros "numeroRomano"]
    , pesNumerosEspecificos = [numeros "numeroRomano"]
    , subComponente = Just compAlinea
    , codigo = "inciso"
  }

compParagrafo ::  ComponenteParseInfo
compParagrafo = cpiMasculino {
      pesNomesCompPlural = [l "paragrafos", l "pars" *** opt (l "."), ltype "SYM_PARAGRAFOS", ltype "SYM_PARAGRAFO" *** ltype "SYM_PARAGRAFO"]
    , pesNomesCompSingularDef = [l "caput", l "cpt" *** opt (l ".")]
    , pesNomesCompSingularIndef = [l "paragrafo", l "par" *** opt (l "."), ltype "SYM_PARAGRAFO"]
    , pesNumerosComp = [variosNumeros "numeroOrdinal", variosNumeros "numeroArabico"]
    , pesNumerosEspecificos = [variosNumeros "numeroOrdinal", variosNumeros "numeroArabicoMaiorQue10"]
    , subComponente = Just compInciso
    , codigo = "paragrafo"
  }  

compArtigo ::  ComponenteParseInfo
compArtigo = cpiMasculino {
      pesNomesCompPlural = [l "artigos", l "arts" *** opt (l ".")]
    , pesNomesCompSingularIndef = [l "artigo", l "art" *** opt (l ".")]
    , pesNumerosComp = [variosNumeros "numeroOrdinal", variosNumeros "numeroArabico"]
    , pesNumerosEspecificos = [numeros "numeroOrdinal",numeros "numeroArabicoMaiorQue10"]
    , subComponente = Just compParagrafo
    , codigo = "artigo"
  }

allComps = [compArtigo,compParagrafo,compInciso,compAlinea,compItem]
forAllComps :: (ComponenteParseInfo  -> Prod) -> GrammarWriter ()
forAllComps f = mapM_ (\c -> tell [f c]) allComps

forAllComps' :: (ComponenteParseInfo  -> GrammarWriter()) -> GrammarWriter ()
forAllComps' f = mapM_ f allComps

preLinkerGrammar = grammar $ do
  "norma" #= opt ( compDirComNome' compArtigo |||
               componente compArtigo ) *** nt0 "norma'"
  "norma'" #= opt ( opt (l ",") *** opt (l "todos" ||| l "todas" ||| l "ambos" ||| l "ambas") *** (l "de" ||| l "do" ||| l "da" )) *** (nt0 "constituicao1988" ||| nt0 "apelidos" ||| nt0 "normaExtenso" )
  "constituicao1988" #= l "constituicao" *** opt (l "federal")
  "apelidos" #= lexSeq ["regimento","interno","do","senado"] *** opt (l "federal") |||
                lexSeq ["regulamento","administrativo","do","senado","federal"] |||
                lexSeq ["ato","das","disposicoes","constitucionais","transitorias"]
  "normaExtenso" #= nt0 "tipoNorma" *** opt (nt0 "qualificadores")
  "tipoNorma" #= nt0 "tnLei" ||| nt0 "tnResolucao"  ||| nt0 "tnDecreto" ||| 
                 nt0 "tnEmendaConstitucional" ||| nt0 "tnMedidaProvisoria" ||| 
                 nt0 "tnCLT" ||| nt0 "tnCF"
  "tnLei" #= ( l "lei" ||| l "leis" ) ***
             opt ( lexAlt ["complementar","complementares","delegada","delegadas"] ) ***
             opt ( lexAlt ["federal","federais","estadual","estaduais","municipal",
                           "municipais","distrital","distritais"] )
  "tnResolucao" #= l "resolucao" ||| l "resolucoes"
  "tnDecreto" #= (l "decreto" ||| l "decretos" ) *** opt (nt0 "hifen" *** l "lei") ***
             opt ( lexAlt ["federal","federais","estadual","estaduais","municipal",
                           "municipais","distrital","distritais"] )
  "tnEmendaConstitucional" #= l "emenda" *** l "constitucional"
  "tnMedidaProvisoria" #= ( l "medida" ||| l "medidas" ) ***
                          ( l "provisoria" ||| l "provisorias" )
  "tnCLT" #= l "clt" ||| lexSeq ["consolidacao","das","leis","do","trabalho"]
  "tnCF" #= l "cf" *** l "/" *** ltype "NUMERO"
  "qualificadores" #= many1 (nt0 "qualificador") ||| l "-" *** many (nt0 "palavraNotE")
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
  "qualNumero" #= opt (l "e") *** opt (nt0 "abrevNumero") *** variosNumeros "numeroArabico"
  forAllComps $ \c -> pn_nomesCompSingular c ::= nomesCompSingularDef c ||| nomesCompSingularIndef c
  forAllComps $ \c -> pn_nomesCompPlural c ::= PE_Choice (pesNomesCompPlural c)
  forAllComps $ \c -> pn_numerosEspecificos c ::= PE_Choice (pesNumerosEspecificos c)

  forAllComps' $ \c -> if c == compItem then return ()
                         else pn_compDirComNome' c ##= listaDe (pn_compDirComNome'_1 c)
  forAllComps' $ \c -> 
    if c == compItem then return ()
    else ( pn_compDirComNome'_1 c ##= (
      if c == compParagrafo then 
        compDirPlural c ||| compDirSingComNome c ||| compDirComNome' compInciso 
      else 
        compDirPlural c ||| compDirSingComNome c
    ) )
  forAllComps $ \c -> 
    pn_compDirPlural c ::= 
        opt (peArtigosPlural c ||| l "nos" ||| l "nas") ***
        opt (l "termos" *** pePrepArtigoPlural c) ***
        nomesCompPlural c ***
        compDirNomeOpt' c
  forAllComps' $ \c ->
    if c == compItem then return () 
    else pn_compDirSingComNome c ##=
           compDirSingPrelim c ***
           nomesCompSingular c ***
           numerosEspecificos c ***
           opt (compDirSingComNome_1 c)
  forAllComps $ \c -> 
    pn_compDirSingComNome_1 c ::= case subComponente c of
        Nothing -> PE_Empty
        Just sc -> (l "," ||| l ";") *** compDirComNome' c
  forAllComps $ \c -> 
    pn_compDirSingPrelim c ::= 
      peArtigosSingular c |||
      l "no" ||| l "na" |||
      ( opt (l "," ||| l ";") ***
        l "nos" *** l "termos" ***
        pePrepArtigoSing c ) |||
      preposicao c
  forAllComps $ \c ->
    pn_preposicao c ::=
      ((l "," ||| l ";") *** pePreposicoes c) ||| opt (pePreposicoes c)
  forAllComps $ \c ->
    pn_compDirNomeOpt' c ::= listaDe (pn_compDirNomeOpt'_1 c)
  forAllComps $ \c ->
    pn_compDirNomeOpt'_1 c ::= 
      if c == compParagrafo then
        compDirPlural c ||| compDirSingNomeOpt c ||| compDirComNome' compInciso
      else 
        compDirPlural c ||| compDirSingNomeOpt c
  forAllComps $ \c ->
    pn_compDirSingNomeOpt c ::= 
      compDirSingPrelim c ***
      opt (nomesCompSingular c) ***
      numerosEspecificos c ***
      opt (compDirSingComNome_1 c)
  forAllComps $ \c ->
    pn_componente c ::= listaDe (pn_compPlural2 c)
  forAllComps $ \c ->
    pn_compPlural2 c ::= compPlural2_a c *** opt (compPlural2_b c)
  forAllComps $ \c ->
    pn_compPlural2_a c ::= compPlural1 c ||| compSing c
  forAllComps $ \c ->
    pn_compPlural2_b c ::= 
      PE_Choice [ many1 (l sep *** compPlural2_a c) *** opt (l sep) | sep <- [",", ";"] ] ***
      opt (compPlural2_c c)
  forAllComps $ \c ->
    pn_compPlural2_c c ::= l "e" *** compPlural2_a c
  forAllComps $ \c ->
    pn_compPlural1 c ::= 
      opt (peArtigosPlural c ||| l "nos" ||| l "nas") ***
      nomesCompPlural c *** numerosComp c
  forAllComps $ \c ->
    pn_compSing c ::= case subComponente c of
     Just sc -> componente sc *** opt (compSing2 c) ||| compSing2 c
     Nothing -> compSing2 c
  forAllComps $ \c ->
    pn_compSing2 c ::=
      opt (lexAlt ["todos","todas","ambos","ambas"] ||| lexSeq ["nos","termos"]) ***
      opt (preposicao c) *** compSing1 c
  forAllComps $ \c ->
    pn_compSing1 c ::=
      opt (peArtigosSingular c ||| l "nos" ||| l "nas") ***
      ( nomesCompSingularDef c ||| nomesCompSingularIndef c *** numerosComp c)

listaDeProds = grammar $ 
    flip mapM_ preLinkerGrammar $ 
      \( _ ::= pe) -> flip mapM_ (ntrefs pe) listaDeProds'
  where 
    listaDeProds' pn@(PN "listaDe" [p]) = do
        pn_listaDe p ##= listaDe' p "," ||| listaDe' p ";"
        flip mapM_ [",",";"] listaDe'Prods
      where
        p_nt = PE_NonTerminal p
        listaDe'Prods sep =
          pn_listaDe' p sep ##= p_nt *** opt (l sep *** p_nt) *** opt (opt (l sep) *** l "e" *** p_nt)
    listaDeProds' _ = return ()

linkerGrammar = optimizePL $ preLinkerGrammar ++ listaDeProds

unions = foldl S.union S.empty

refsMade = unions $ map (ntrefs . (\ (_ ::= pe) -> pe)) linkerGrammar
refsDef = S.fromList $ map (\ (pn ::= _) -> pn) linkerGrammar
undefs = S.difference refsMade  refsDef
notUsed = S.difference refsDef refsMade
nonTerminals = lexemesPL linkerGrammar

stats = do
  putStrLn "Undefined Non-Terminals:"
  flip mapM_ (S.toAscList undefs) $ \r -> putStrLn $ "    " ++ pnPretty r
  putStrLn "------------------------"
  putStrLn "Unused Non-Terminals:"
  flip mapM_ (S.toAscList notUsed) $ \r -> putStrLn $ "    " ++ pnPretty r
  putStrLn "------------------------"
  putStrLn "Terminals:"
  mapM_ (\x -> putStrLn $ "   " ++ show x) nonTerminals
  putStrLn "------------------------"
  putStrLn "Results: "
  putStrLn "----START GRAMMAR------"
  putStrLn $ prodsANTLR linkerGrammar
  putStrLn "----END GRAMMAR--------"

writeGrammar = do
  lexerRules <- readFile "LinkerLexerRules.g4"
  basicGrammar <- readFile "BasicLinkerGrammarRules.g4"
  writeFile "LexmlLinker.g4" $
    "grammar LexmlLinker;\n\n" ++
    lexerRules ++ "\n" ++
    basicGrammar ++ "\n" ++
    prodsANTLR linkerGrammar



