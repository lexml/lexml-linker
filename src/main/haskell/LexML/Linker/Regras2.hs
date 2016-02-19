{-# LANGUAGE FlexibleContexts #-}

module LexML.Linker.Regras2 (parseCases) where

import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import LexML.Linker.Decorator (DecorationMap, addURN)
import Data.Typeable
import LexML.Linker.LexerPrim (Token, TokenData(..), tokenDataLength)
import LexML.URN.Utils
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
import qualified Data.Set as S
import Data.List(intercalate)

type SelectF = URNLexML -> URNLexML

data NomeComponente = NC_Simple String | NC_Abrev String | NC_Simbolo TokenData | NC_Outro (LinkerParserMonad (Pos, Maybe SelectF))

type SimpleNumeroParser = LinkerParserMonad (Pos,Pos,Integer)
type NumeroParser = LinkerParserMonad (Pos,Pos,[Integer])

data ComponenteParseInfo = CPI {        
      descricaoComp :: String,
      levelComp :: Int,
      nomesCompPlural :: [NomeComponente],      
      artigosPlural :: [String],
      nomesCompSingular :: [NomeComponente],
      artigosSingular :: [String],
      numerosComp :: [NumeroParser],
      numerosEspecificos :: [NumeroParser],
      subComponente :: Maybe ComponenteParseInfo,
      preposicoes :: [String],
      selectComp :: [Integer] -> SelectF,
      selectDefault :: Maybe SelectF
  }

log ::  String -> LinkerParserMonad ()
log = logRegras -- msg = return () -- lift $ tell $ msg ++ "\n"

log' :: ComponenteParseInfo -> String -> String -> LinkerParserMonad ()
log' cpi wh msg = do
  lh <- try (eof >> return Nothing) <|> (lookAhead anyToken >>= return . Just)
  let indent = replicate ((levelComp cpi) * 2) ' '
  log $ indent ++ wh ++ "[" ++  descricaoComp cpi ++ "]: " ++ msg ++ " (lh: " ++ show lh ++ ")" 

log'' :: String -> LinkerParserMonad ()
log'' msg = do
  lh <- try (eof >> return Nothing) <|> (lookAhead anyToken >>= return . Just)
  log $ msg ++ " (lh: " ++ show lh ++ ")" 

cpiFeminino = CPI {
      descricaoComp = ""
    , levelComp = 0
    , nomesCompPlural = []
    , artigosPlural = ["as"]
    , nomesCompSingular = []
    , artigosSingular = ["a"]
    , numerosComp = []
    , numerosEspecificos = []
    , subComponente = Nothing
    , preposicoes = ["da"]
    , selectComp = const id
    , selectDefault = Nothing
}

cpiMasculino = CPI {
      descricaoComp = ""
    , levelComp = 0
    , nomesCompPlural = []
    , artigosPlural = ["os"]
    , nomesCompSingular = []
    , artigosSingular = ["o"]
    , numerosComp = []
    , numerosEspecificos = []
    , subComponente = Nothing
    , preposicoes = ["do"]
    , selectComp = const id
    , selectDefault = Nothing
}


compItem ::  ComponenteParseInfo
compItem = cpiMasculino {
      descricaoComp = "item"
    , levelComp = 4
    , nomesCompPlural = [NC_Simple "itens"]
    , nomesCompSingular = [NC_Simple "item"]
    , numerosComp = [numeros numeroArabico]
    , selectComp = U.selecionaItem
  }

compAlinea ::  ComponenteParseInfo
compAlinea = cpiFeminino {
      descricaoComp = "alinea"
    , levelComp = 3
    , nomesCompPlural = [NC_Simple "alíneas", NC_Simple "alineas", NC_Simple "letras"]
    , nomesCompSingular = [NC_Simple "alínea", NC_Simple "alinea", NC_Simple "letra"]
    , numerosComp = [numeros numeroAlfabeto, numeros numeroArabico, numeros numeroRomano]
    , numerosEspecificos = [numeros numeroAlfabeto']
    , subComponente = Just compItem
    , selectComp = U.selecionaAlinea
  }


compInciso ::  ComponenteParseInfo
compInciso = cpiMasculino {
      descricaoComp = "inciso"
    , levelComp = 2
    , nomesCompPlural = [NC_Simple "incisos", NC_Abrev "incs"]
    , nomesCompSingular = [NC_Simple "inciso", NC_Abrev "inc"]
    , numerosComp = [numeros numeroArabico, numeros numeroRomano]
    , numerosEspecificos = [numeros numeroRomano]
    , subComponente = Just compAlinea
    , selectComp = U.selecionaInciso
  }

compParagrafo ::  ComponenteParseInfo
compParagrafo = cpiMasculino {
      descricaoComp = "paragrafo"
    , levelComp = 1
    , nomesCompPlural = [NC_Simple "parágrafos", NC_Simple "paragrafos", NC_Abrev "pars", NC_Simbolo Paragrafos, NC_Outro parseParagrafos]
    , nomesCompSingular = [NC_Outro caput, NC_Simple "parágrafo", NC_Simple "paragrafo", NC_Abrev "par", NC_Simbolo Paragrafo]
    , numerosComp = [variosNumeros' numeroOrdinal, variosNumeros' numeroArabico]
    , numerosEspecificos = [variosNumeros' numeroOrdinal, variosNumeros' (numeroArabicoMaiorQue 10)]
    , subComponente = Just compInciso
    , selectComp = U.selecionaParagrafo
    , selectDefault = Just $ U.selecionaCaput
  }  

compArtigo ::  ComponenteParseInfo
compArtigo = cpiMasculino {
      descricaoComp = "artigo"
    , nomesCompPlural = [NC_Simple "artigos", NC_Abrev "arts"]
    , nomesCompSingular = [NC_Simple "artigo", NC_Abrev "art"]
    , numerosComp = [variosNumeros' numeroOrdinal, variosNumeros' numeroArabico]
    , numerosEspecificos = [numeros numeroOrdinal, numeros (numeroArabicoMaiorQue 10)]
    , subComponente = Just compParagrafo
    , selectComp = U.selecionaArtigo
  }

caput :: LinkerParserMonad (Pos,Maybe SelectF)
caput = do
  inicio <- constanteI "caput" <|> (constanteI "cpt" >>= \p -> optional ponto >> return p)
  return (inicio, Just U.selecionaCaput)

complementoToInteger :: String -> Integer
complementoToInteger = foldl (\n c -> n * (1 + fromIntegral (fromEnum 'Z' - fromEnum 'A')) + (fromIntegral (fromEnum c - fromEnum 'A'))) 0  . fixMsd . map toUpper
  where
    fixMsd (c:cs@(_:_)) = toEnum (fromEnum c + 1) : cs
    fixMsd cs = cs

numeros ::  SimpleNumeroParser -> NumeroParser
numeros m = do
  log'' $ "                   numeros: starting"
  (i,f,n) <- m
  (comp,ff) <- try $ option (Nothing,f) $ do
    hifen
    (pos,p) <- anyPalavra
    return $ (Just $ complementoToInteger p + 1,pos)
  log'' $ "                   numeros: result: " ++ show (i,ff,n)
  return (i,ff,n : maybeToList comp)


variosNumeros' ::  SimpleNumeroParser -> NumeroParser
variosNumeros' m = variosNumeros m [numero >>= \ (p,(n,_)) -> return (p,p,n), numeroAlfabeto]

variosNumeros ::  SimpleNumeroParser -> [SimpleNumeroParser] -> NumeroParser
variosNumeros primeiro outros = do
  (inicio,fim1,n) <- primeiro 
  nl <- many $ try $ do
        hifen
        (_,fim,n) <- choice $ map try $ outros
        return (fim,n)
  let fim2 = case nl of 
                [] -> fim1
                _  -> fst $ last nl
  return (inicio,fim2, n : snd (unzip nl))
{-
listaDe :: LinkerParserMonad [a] -> LinkerParserMonad [a]
listaDe m = do
  log'' "          listaDe: start"
  res1 <- m
  log'' "          listaDe: res1 obtained"
  resl <- try (parseSeparator >>= sepBy m) <|> return []
  log'' "          listaDe: resl obtained. returning."
  return $ concat $ res1:resl
-}
listaDe :: LinkerParserMonad [a] -> LinkerParserMonad [a]
listaDe m = do
  log'' $ "        listaDe: starting"
  res1 <- m
  log'' "          listaDe: res1 obtained"
  (psep,resl) <- option (parseSeparator >> return (),[]) $ try $ do
    log'' $ "          listaDe(resl): starting"
    sep <- parseSeparator
    log'' $ "          listaDe(resl): sep parsed"
    resl <- sepBy1 (try m) sep
    log'' $ "          listaDe(resl): resl parsed"
    return (sep,concat resl)
  log'' $ "          listaDe: resl parsed"
  resl' <- option resl $ try $ do
              optional psep
              log'' $ "          listaDe: checking for \"e\""
              constanteI "e"
              log'' $ "          listaDe: \"e\" is present"
              r <- m 
              log'' $ "          listaDe: m parsed after \"e\""
              return (resl ++ r)
  return (res1 ++ resl')

{-- listaDe' :: LinkerParserMonad (s,[a]) -> (a -> LinkerParserMonad [a]) -> LinkerParserMonad [a]
listaDe' s m = do
  log'' "          listaDe: start"
  res1 <- m s
  log'' "          listaDe: res1 obtained"
  resl <- try (parseSeparator >>= sepBy m) <|> return []
  log'' "          listaDe: resl obtained. returning."
  return $ concat $ res1:resl --}

parseComponenteDireto ::  ComponenteParseInfo -> ParseCase2
parseComponenteDireto = parseComponenteDireto' True 

parseComponenteDireto' ::  Bool -> ComponenteParseInfo -> ParseCase2
parseComponenteDireto' reqNome cpi = do
    log "parseComponenteDireto'" $ "start reqNome = " ++ show reqNome
    l <- listaDe (try parseComponenteDiretoPlural <|> try parseComponenteDiretoSingular <|> parseSubCompIfOptional)
    return l
  where
    parseSubCompIfOptional = 
      case selectDefault cpi of
        Just uf -> do
          l <- tryParseSubComponent
          return [(i,f, u . uf) | (i,f,u) <- l]
        Nothing -> fail "not optional"
    tryParseSubComponent = 
      case subComponente cpi of
        Nothing -> fail "no subcomponent"
        Just sc -> parseComponenteDireto' True sc
    log = log' cpi
    sc = selectComp cpi
    parseComponenteDiretoSingular = do
      log "parseComponenteDiretoSingular" "start"
      optional $ try $ choice $ (map (\c -> constanteI c >> return ()) (artigosSingular cpi ++ ["no","na"]) ++
        [ try $ do 
             log "parseComponenteDiretoSingular(nos termos)" "start"
             optional parseSeparator
             constanteI "nos"
             log "parseComponenteDiretoSingular(nos termos)" "looking for termos"
             constanteI "termos"
             log "parseComponenteDiretoSingular(nos termos)" $ "looking for preposicoes :" ++ show (preposicoes cpi)
             choice (map constanteI $ (preposicoes cpi ++ artigosSingular cpi))
             log "parseComponenteDiretoSingular(nos termos)" "end"
             return (), 
          try $ do log "parseComponenteDiretoSingular(preposicao)" "start"
                   parsePreposicao cpi
                   log "parseComponenteDiretoSingular(preposicao)" "end"])
      (minicio, mselect) <- (if reqNome then id else option (Nothing,Nothing)) (parseNomes (nomesCompSingular cpi) >>= \ (a,b) -> return (Just a,b))
      log "parseComponenteDiretoSingular" $ "minicio = " ++ show minicio
      (inicio2,fim,num) <- choice $ map try $ numerosEspecificos cpi
      log "parseComponenteDiretoSingular" $ "(inicio2,fim,num) = " ++ show (inicio2,fim,num)
      let inicio = maybe inicio2 id minicio
          sc' = sc num     
      subcomps <- option [] $ try $ case subComponente cpi of
          Nothing -> return []
          Just subc -> do
            log "parseComponenteDiretorSingular" $ "parsing for subcomponent (,)"
            virgula <|> pontoevirgula
            log "parseComponenteDiretorSingular" $ "parsing for subcomponent"
            parseComponenteDireto' True subc
      case subcomps of
        [] -> return [(inicio,fim,sc')]
        (_,fim',sf):r -> return $ (inicio,fim',sf . sc') : [(i,f,sf . sc') | (i,f,sf) <- r]
        
    parseComponenteDiretoPlural = do
      log "parseComponenteDiretoPlural" "starting"
      optional (try $ choice $ map constanteI $ artigosPlural cpi ++ ["nos","nas"])
      optional $ try (constanteI "termos" >> choice (map constanteI $ preposicoes cpi ++ artigosPlural cpi) )
      (inicio, mselect) <- parseNomes $ nomesCompPlural cpi             
      log "parseComponenteDiretoPlural" "nome parsed"
      (_,fim,u):r <- parseComponenteDireto' False cpi
      log "parseComponenteDiretoPlural" "rest parsed"
      return $ (inicio,fim,u):r

parseComponente ::  ComponenteParseInfo -> ParseCase2
parseComponente cpi = do
  log' cpi "parseComponente" "start" 
  listaDe $ try $ parseComponente1 cpi
--  log' cpi "parseComponente" "end"

parseComponente1 ::  ComponenteParseInfo -> ParseCase2
parseComponente1 cpi = parseComponentePlural2 cpi
--parseComponente1 cpi = try (log' cpi "parseComponente1" "starting A" >> parseComponentePlural1 cpi) 
--                      <|> (log' cpi "parseComponente1" "starting B" >> parseComponentePlural2 cpi)

parseComponentePlural1 ::  ComponenteParseInfo -> ParseCase2
parseComponentePlural1 cpi@(CPI { nomesCompPlural = nomesCompPlural,
                                  artigosPlural = artigosPlural,
                                  numerosComp = numerosComp,
                                  selectComp = selectComp }) = do
    log' cpi "parseComponentePlural1" "starting"
    inicio1 <- option Nothing $ fmap Just $ choice $ map constanteI $ artigosPlural ++ ["nos","nas"]
    log' cpi "parseComponentePlural1" "artigo parsed"
    (inicio2, mselect) <- parseNomes nomesCompPlural 
    let inicio = maybe inicio2 id inicio1
    log' cpi "parseComponentePlural1"  $ "nome parsed, inicio = " ++ show inicio
    case mselect of
      Just s -> return [(inicio,inicio,s)]
      Nothing -> do
        nums <- parseListaNumeros numerosComp
        log' cpi "parseComponentePlural1" $ "nums parsed = " ++ show nums ++ ", returning"
        let nums2 = case nums of ((_,fim,n):nl) -> (inicio,fim,n) : nl
        return [(inicio,fim,selectComp n) | (inicio,fim,n) <- nums2 ]

parseComponentePlural2 ::  ComponenteParseInfo -> ParseCase2
parseComponentePlural2 cpi = do
  log' cpi "parseComponentePlural2" "starting"
  c1 <- try (parseComponentePlural1 cpi) <|> parseComponenteSingular cpi 
  log' cpi "parseComponentePlural2" $ "c1 parsed"
  cl <- option [] $ try $ do
          log' cpi "parseComponentePlural2(cl)" "starting"
          sep <- parseSeparator    
          log' cpi "parseComponentePlural2(cl)" "sep parsed"
          cl <- sepBy (try (parseComponentePlural1 cpi) <|> parseComponenteSingular cpi) sep
          log' cpi "parseComponentePlural2(cl)" $ "cl parsed. "
          optional sep
          log' cpi "parseComponentePlural2(cl)" "optional sep parsed"
          return cl
  cl' <- option cl $ try $ do
            log' cpi "parseComponentePlural2(cl,e)" "starting"
            constanteI "e" 
            log' cpi "parseComponentePlural2(cl,e)" "e parsed"
            r <- try (parseComponentePlural1 cpi) <|> parseComponenteSingular cpi
            log' cpi "parseComponentePlural2(cl,e)" $ "componente singular parsed"
            return (cl ++ [r])
  log' cpi "parseComponentePlural2" "returning"
  return $ concat (c1 : cl')

parseComponenteSingular ::  ComponenteParseInfo -> ParseCase2
parseComponenteSingular cpi = combineM' subComponente' (
        do log' cpi "parseComponenteSingular"  "starting"
           optional . choice $ map constanteI ["todos","todas","ambos","ambas"] ++
                               [constanteI "nos" >> constanteI "termos"]
           log' cpi "parseComponenteSingular"  "parsing preposicao"
           optional (parsePreposicao cpi) 
           log' cpi "parseComponenteSingular" "preposicao parsed"
           parseComponenteSingular1 cpi
           ) (selectDefault cpi)
  where
    subComponente' = maybe (log' cpi "parseComponenteSingular"  "no default" >> return []) parseComponente $ subComponente cpi
    

parseComponenteSingular1 :: ComponenteParseInfo -> LinkerParserMonad SingleParseCaseResult2
parseComponenteSingular1 cpi@(CPI { nomesCompSingular = nomesCompSingular,
                               artigosSingular = artigosSingular,
                               numerosComp = numerosComp,
                               selectComp = selectComp }) = do
    log' cpi "parseComponenteSingular1" "start"
    optional $ choice $ map constanteI $ artigosSingular ++ ["no", "na"]
    log' cpi "parseComponenteSingular1" "artigo parsed"
    (inicio, mselect) <- parseNomes nomesCompSingular
    log' cpi "parseComponenteSingular1" $ "nome parsed:  " ++ show inicio
    case mselect of
      Just s -> return (inicio,inicio,s)
      Nothing -> do
        (_,fim,num) <- choice (map try numerosComp) 
        log' cpi "parseComponenteSingular1" $ "num parsed: " ++ show (fim,num)
        return (inicio,fim,selectComp num)
    
parsePreposicao :: ComponenteParseInfo -> LinkerParserMonad ()
parsePreposicao (CPI { preposicoes = preposicoes }) = (parseSeparator >> optional prep) <|> prep
  where
    prep = (choice $ map constanteI preposicoes) >> return ()

intervalo :: [NumeroParser] -> LinkerParserMonad ([(Pos,Pos,[Integer])],NumeroParser)
intervalo numeros = do
  log'' $ "              intervalo: starting"
  (num1,numero) <- choice [ try (numero >>= \r -> return (r ,numero)) | numero <- numeros ]
  log'' $ "              intervalo: num1 = " ++ show num1
  numl <- option [num1] $ try $ do
      constanteI "a"
      log'' $ "              intervalo: a parsed"
      num2 <- numero
      log'' $ "              intervalo: num2 = " ++ show num2
      return [num1,num2]
  return (numl,numero)

intervalo' :: NumeroParser -> LinkerParserMonad [(Pos,Pos,[Integer])]
intervalo' numero = do
  num1 <- numero
  numl <- option [num1] $ try $ do
      constanteI "a"
      num2 <- numero
      return [num1,num2]
  return numl

parseListaNumeros :: [NumeroParser] -> LinkerParserMonad [(Pos,Pos,[Integer])]
parseListaNumeros numeros = do
  log'' $ "        parseListaNumeros: starting"
--  (num1,numero) <- choice [ try (numero >>= \r -> return (r ,numero)) | numero <- numeros ]
  (nums1,numero) <- intervalo numeros
  log'' $ "        parseListaNumeros: num1 parsed : " ++ show nums1
  numl <- option [] $ try (do
    log'' $ "          parseListaNumeros(numl): starting"
    sep <- parseSeparator
    log'' $ "          parseListaNumeros(numl): sep parsed"
    numl <- sepBy (intervalo' numero) sep
    log'' $ "          parseListaNumeros(numl): numl parsed: " ++ show numl
    optional sep
    return $ concat numl)
  log'' $ "          parseListaNumeros: numl parsed"
  numl' <- option numl (try (constanteI "e" >> (try (intervalo' numero >>= \r -> return $ (numl ++ r)) <|>
                                                (constanteI "seguintes" >> return numl) )))
  return (nums1 ++ numl')

parseSeparator :: LinkerParserMonad (LinkerParserMonad ())
parseSeparator = choice [ s >> return (s >> return ()) | s <- [virgula, pontoevirgula]]

parseNomes :: [NomeComponente] -> LinkerParserMonad (Pos, Maybe SelectF)
parseNomes = choice . map (try . parseNome)

parseNome :: NomeComponente -> LinkerParserMonad (Pos, Maybe SelectF)
parseNome (NC_Simple n) = noSelectF $ constanteI n
parseNome (NC_Abrev n) = noSelectF (constanteI n >>= \p -> optional ponto >> return p)
parseNome (NC_Simbolo td) = noSelectF $ fmap fst $ lToken (\td' -> if td == td' then Just () else Nothing)
parseNome (NC_Outro m) = m

parseParagrafos :: LinkerParserMonad (Pos, Maybe SelectF)
parseParagrafos = noSelectF $ do
  log'' "parseParagrafos: start"
  (s,_) <- parseNome (NC_Simbolo Paragrafo) 
  log'' $ "parseParagrafos: parsed first: " ++ show s
  parseNome (NC_Simbolo Paragrafo) 
  log'' $ "parseParagrafos: parsed second"
  return s

noSelectF :: Monad m => m Pos -> m (Pos,Maybe SelectF) 
noSelectF m = m >>= \p -> return (p, Nothing)

numero' :: LinkerParserMonad Token
numero' = tokenPrim show (\ _ ((x,y),_) _ -> newPos "" x y) f
  where f t@((_,_),Numero _ _) = return t
        f _ = fail $ "expecting number"

numero :: LinkerParserMonad ((Line,Column,Int),(Integer,String))
numero = try $ do
  log'' $ "                   numero: parsing"
  (p,td) <- anyTokenData
  log'' $ "                   numero: next token: " ++ show (p,td)
  case td of
    Numero n s -> log'' "                        numero: returning"  >> return (p,(n,s))
    _ -> log'' ("not a number:  " ++ show (p,td)) >> fail "not a number"
--lToken numero'
--  where
--    numero' (Numero n s) = log "numero found" >> return (n,s)
--    numero' _ = log "numero not found" >> (fail $ "numero expected")

digitos :: Int -> LinkerParserMonad (Pos,String)
digitos n = lToken digitos'
  where
    digitos' (Numero _ s) | length s == n = return s
    digitos' _ = fail $ show n ++ " digits expected"

numeroArabico :: LinkerParserMonad (Pos,Pos,Integer)
numeroArabico = 
    do --log $ "             numeroArabico: starting"
       (inicio,(_,s)) <- numero
       --log $ "             numeroArabico: numero parsed: " ++ show (inicio,s)
       l <- many $ try $ do
              --log $ "               numeroArabico: many: starting"
              ponto
              --log $ "               numeroArabico: many: ponto parsed"
              r <- digitos 3
              --log $ "               numeroArabico: many: digitos parsed"
              return r
       let (posl,sl) = unzip l
       let num = read $ concat $ s : sl
           fim = if null posl then inicio else last posl
       --log $ "             numeroArabico: returning " ++ show (inicio,fim,num)
       (((indicadorOrdinal >> return ()) <|> (constanteI "o" >> return ())) >> fail "is ordinal") <|> return ()
       return (inicio,fim,num)

numeroArabicoMaiorQue :: Integer -> LinkerParserMonad (Pos,Pos,Integer)
numeroArabicoMaiorQue lb = try $
  do res@(i,f,n) <- numeroArabico
     if n > lb then return res else fail $ "num " ++ show n ++ ", less than " ++ show lb
       
numeroOrdinal :: LinkerParserMonad (Pos,Pos,Integer)
numeroOrdinal = ordinal <|> ordinal' <|> extenso
  where
    ordinal = do
      (p1,(r,_)) <- numero
      p2 <- option p1 ((fmap fst indicadorOrdinal) <|> constanteI "o" <|> (constanteI "a" >>= \p -> notFollowedBy numero' >> return p))
      --p2 <- option p1 (indicadorOrdinal <|> constanteI "o" <|> (constanteI "a" >>= \p -> return () >> return p))
      return (p1,p2,r)
    ordinal' = lToken f >>= \ (p,n) -> return (p,p,n)
      where
        f (Ordinal n _) = return n
        f _ = fail "ordinal expected"  
    extenso = parseLookup [
      ("único",1),("unico",1),("primeiro",1),
      ("segundo",2),("terceiro",3),("quarto",4),
      ("quinto",5),("sexto",6),("sétimo",7),("setimo",7),
      ("oitavo",8),("nono",9),("décimo",10),("decimo",10) ] >>= \ (p, r) -> return (p,p,r)

abrevNumero :: LinkerParserMonad (Line,Column,Int)
abrevNumero = try (choice $ map constanteI $ [ "n.", "n.o.", "no", "número", "nos" ]) <|> 
                (do p <- constanteI "n" 
                    optional ponto
                    try (optional (indicadorOrdinal >> many indicadorOrdinal >> optional (constanteI "s") >> optional ponto))
                    return p)

numeroRomano :: LinkerParserMonad (Pos,Pos,Integer)
numeroRomano = palavraI parseStringNumeroRomano >>= \ (p,n) -> return (p,p,n)

parseStringNumeroRomano :: Text.Parsec.Prim.Stream s m Char => Text.Parsec.Prim.ParsecT s u m Integer
parseStringNumeroRomano = do
--    milhares <- fmap length (many $ char 'm')
    let milhares = 0
--    centenas <- pcentenas
    let centenas = 0
    dezenas <- pdezenas
    unidades <- punidades
    let res = milhares * 1000 + centenas * 100 + dezenas * 10 + unidades
    if res <= 0 then fail "Not a roman number" else return (fromIntegral res)

pcentenas :: Text.Parsec.Prim.Stream s m Char => Text.Parsec.Prim.ParsecT s u m Int
pcentenas = choice [
              try (string "cm") >> return 9,
              try (string "d" >> fmap length (manyMN 0 3 $ char 'c') >>= \n -> return (5 + n)),
              try (string "cd") >> return 4,
              try (fmap length (manyMN 1 3 $ char 'c')),
              return 0]

pdezenas :: Text.Parsec.Prim.Stream s m Char => Text.Parsec.Prim.ParsecT s u m Int
pdezenas = choice [
              try (string "xc") >> return 9,
              try (string "l" >> fmap length (manyMN 0 3 $ char 'x') >>= \n -> return (5 + n)),
              try (string "xl") >> return 4,
              try (fmap length (manyMN 1 3 $ char 'x')),
              return 0 ]

punidades :: Text.Parsec.Prim.Stream s m Char => Text.Parsec.Prim.ParsecT s u m Int
punidades = choice [
              try (string "ix") >> return 9,
              try (char 'v' >> fmap length (manyMN 0 3 $ char 'i') >>= \n -> return (5 + n)),
              try (string "iv") >> return 4,
              try (fmap length (manyMN 1 3 $ char 'i')),
              return 0 ]

manyMN _ 0 f = return []
manyMN 1 1 f = fmap (\x -> [x]) f
manyMN 0 1 f = try (fmap (\x -> [x]) f) <|> return []
manyMN 0 max f = do
  (try f >>= \r -> manyMN 0 (max - 1) f >>= \rs -> return (r:rs)) <|> return []
manyMN min max f = do
  r <- f
  rs <- manyMN (min - 1) (max - 1) f 
  return (r:rs)

numeroAlfabeto :: LinkerParserMonad (Pos,Pos,Integer)
numeroAlfabeto = do
    minicio1 <- option Nothing (fmap Just aspasDuplas)
    (p,n) <- letra
    case minicio1 of
      Nothing -> return (p,p,n)
      Just inicio -> do
        fim <- aspasDuplas
        return (inicio,fim,n)
  where
    aspasDuplas = simbolo (Simbolo '"')
    letra = lToken f >>= \(p,n) -> return (p,n)
      where
        f (Palavra [c]) | isAlpha c = return $ toInteger $ ord (toLower c) - (ord 'a') + 1
        f _ = fail "expected letter"

numeroAlfabeto' :: LinkerParserMonad (Pos,Pos,Integer)
numeroAlfabeto' = do
    inicio <- aspasDuplas
    (p,n) <- letra
    fim <- aspasDuplas
    return (inicio,fim,n)
  where
    aspasDuplas = simbolo (Simbolo '"')
    letra = lToken f >>= \(p,n) -> return (p,n)
      where
        f (Palavra [c]) | isAlpha c = return $ toInteger $ ord (toLower c) - (ord 'a') + 1
        f _ = fail "expected letter"

norma ::  ParseCase2
norma = (try (parseComponenteDireto compArtigo) <|> parseComponente compArtigo) `combineM` norma'

  
norma' ::  ParseCase2
norma' = do
  log'' $ "norma': starting"
  optional $ do
    optional virgula
    optional $ choice $ map constanteI ["todos","todas","ambas","ambos"]
    choice $ map constanteI ["de","do","da"]
  log'' $ "norma': parsing norma"
  cs <- shouldParseConstituicaoSimples
  if cs then try constituicao1988 <|> try apelidos <|> normaExtenso
        else try apelidos <|> normaExtenso
--  try constituicao <|>  try normaExtenso -- not yet: <|>  leiApelido 

constituicao1988 :: ParseCase2
constituicao1988 = do
  log'' $ "constituicao1988: starting"
  (inicio,fim) <- try (do i <- nomeProprio "Constituição" ; f <- option i (constanteI "federal") ; return (i,f)) <|>
                  (do i <- constanteI "constituicao" ; f <- constanteI "federal" ; return (i,f))
  return [(inicio,fim,U.selecionaConstituicao' (Just 1988) (Just (10,5)) Nothing Nothing)]

constituicao ::  ParseCase2
constituicao = do
  log'' $ "constituicao: start "
  inicio <- nomeProprio "Constituição" <|> constanteI "constituicao"
  log'' $ "constituicao: looking for federal"
  fim <- option inicio (constanteI "federal" <|> constanteI "estadual")
  log'' $ "constituicao: looking for qualificadores"
  quals <- parseQualificadores' False
  log'' $ "constituicao: quals = " ++ show quals
  urnContexto <- getUrnContexto
  let (CQ{ cqData = mdata, cqMunicipio = mmunicipio, cqEstado = mestado, cqUltimaPos = mpos},_) = consolidaQualificadores' ["constituicao"] quals
      (ano,mes,vig) = case mdata of
          Nothing -> (Nothing, Nothing,maybe Nothing algumaData urnContexto)
          Just (a,md) -> (Just a,md,Nothing)
  log'' $ "constituicao: ano = " ++ show ano ++ ", mes = " ++ show mes ++ ", estado = " ++ show mestado
  return [(inicio,maybe fim id mpos,U.selecionaConstituicao' ano mes mestado vig)]

leiApelido ::  ParseCase2
leiApelido = try leiApelido1 <|> leiApelido2

leiApelido1 ::  ParseCase2
leiApelido1 = do
    inicio1 <- try (fmap Just (constanteI "antigo")) <|> return Nothing
    inicio <- constantesI ["código", "codigo"] >>= \p -> return $ maybe p id inicio1
    (fim1,ap) <- constantesI' apelidos
    fim2 <- try (constanteI "anterior") <|> return fim1
    option [(inicio,fim2,U.selecionaLeiApelido ["codigo",ap])] $ try $ do 
        virgula
        (_,f,urn):rl <- normaExtenso
        return $ (inicio,f,urn):rl
  where
    apelidos = [
        ("civil"),
        ("penal")
      ]

leiApelido2 ::  ParseCase2
leiApelido2 = try clt1 <|> clt2
  where
    clt1 = do
      inicio <- constanteI "consolidação" <|> constanteI "consolidacao"
      constanteI "das"
      constanteI "leis"
      constanteI "do"
      fim <- constanteI "trabalho"
      return [(inicio,fim,\_ -> U.apelidoCLT)]
    clt2 = do
      p <- constanteI "clt"
      return [(p,p,\_ -> U.apelidoCLT)]

apelidos :: ParseCase2
apelidos = choice $ map try [
     apelidosSimples
  ]


apelidosSimples :: ParseCase2
apelidosSimples = do
    log'' $ "apelidosSimples: iniciando"
    r <- choice $ map (\ (l,u) -> try $
                            do log'' $ "apelidosSimples: tentando: " ++ intercalate "," l
                               (i,f) <- constantesI2 l
                               log'' $ "apelidosSimples: consegui: " ++ intercalate "," l
                               return [(i,f,u)]) listaApelidosSimples
    log'' $ "apelidosSimples: ok"
    return r
  where
    listaApelidosSimples = [
        (["regimento","interno","do","senado","federal"],\_ -> U.apelidoRegimentoInternoSenado)
      , (["regimento","interno","do","senado"],\_ -> U.apelidoRegimentoInternoSenado)
      , (["regulamento","administrativo","do","senado", "federal"],\_ -> U.apelidoRegulamentoAdministrativoSenado)
      , (["regulamento","administrativo","do","senado"],\_ -> U.apelidoRegulamentoAdministrativoSenado)
      , (["ato","das","disposicoes","constitucionais","transitorias"],\_ -> U.atoDisposicoesConstitucionaisTrans)
      , (["ato","das","disposições","constitucionais","transitórias"],\_ -> U.atoDisposicoesConstitucionaisTrans)
      ]


normaExtenso ::  ParseCase2
normaExtenso = do
  log'' $ "normaExtenso: start"
  (inicio,fim,tipo,qualsNorma,precisaAutoConv,precisaQualificadores) <- try tipoNorma <|> try apelido <|> casosEspeciais
  log'' $ "normaExtenso: tipo = " ++ show tipo
  quals' <- parseQualificadores' precisaQualificadores
  let quals = [(inicio,fim,q) | q <- qualsNorma ] ++ quals'
  log'' $ "normaExtenso: qualificadores = " ++ show quals
  let r = consolidaQualificadores tipo quals
  log'' $ "normaExtenso: resultado da consolidacao: " ++ show (length r)
  if null r then fail $ "missing qualificadores" else return ()
  let ((i,f,t):rl) = r
  log'' $ "normaExtenso: retornando " ++ show (1 + length rl)
  t' <- if not precisaAutoConv then return t else do
          mc <- getUrnContexto
          case mc of 
            Just urn | ehAutoridadeConvencionada (t urn) -> return t
                     | otherwise -> case autoridadeConvencionada urn of
                                      Just ac -> return $ substituiAutoridadeConvencionada ac . t
                                      Nothing -> fail $ "Sem autoridade convencionada"
            _ -> fail $ "Sem contexto"
  return $ (inicio,f,t'):rl

data QualificadorNorma = NLD_Numero [Integer] | NLD_Data !Int (Maybe (Int,Int)) | NLD_Municipio [String] [String] | NLD_Estado [String] | NLD_Autoridade Autoridade | NLD_Esfera AutoridadeConvencionada deriving (Eq,Ord,Show)

type QualificadorParser = LinkerParserMonad (Pos,Pos,QualificadorNorma)

parseQualIgnore :: LinkerParserMonad ()
parseQualIgnore = try $ do
  log'' "parseQualIgnore: starting"
  hifen
  many $ palavraFilter (/= "e")
  log'' "parseQualIgnore: ending"
  return () 

parseQualificador :: LinkerParserMonad (Pos,Pos,QualificadorNorma)
parseQualificador = try qualificadores1 <|> qualificadores2
  where
    qualificadores1 = do
      optional (virgula <|> pontoevirgula)
      choice $ map try [parseQualNumero, parseQualData, parseQualData2, parseQualMunicipio, parseQualEstado ]
    qualificadores2 = parseQualAutoridade

parseQualificadores :: LinkerParserMonad [(Pos,Pos,QualificadorNorma)]
parseQualificadores = parseQualificadores' True

parseQualificadores' :: Bool -> LinkerParserMonad [(Pos,Pos,QualificadorNorma)]
parseQualificadores' obrig = (if obrig then many1 else many) (choice [try parseQualificador >>= return . Just, parseQualIgnore >> return Nothing ]) >>= return . catMaybes

data ContextoQualificador = CQ { cqData :: Maybe (Int, Maybe (Int,Int)), cqMunicipio :: Maybe ([String],[String]), 
                                 cqEstado :: Maybe [String], cqUltimaPos :: Maybe Pos,
                                 cqAutoridade :: Maybe Autoridade } deriving (Show)

contextoInicial = CQ { cqData = Nothing, cqMunicipio = Nothing, cqEstado = Nothing, cqUltimaPos = Nothing, cqAutoridade = Nothing }

consolidaQualificadores :: [String] -> [(Pos,Pos,QualificadorNorma)] -> [(Pos,Pos,URNLexML -> URNLexML)]
consolidaQualificadores = (snd .) . consolidaQualificadores'

consolidaQualificadores' :: [String] -> [(Pos,Pos,QualificadorNorma)] -> (ContextoQualificador,[(Pos,Pos,URNLexML -> URNLexML)])
consolidaQualificadores' tipoNorma = foldr consolida (contextoInicial,[])
  where
    consolida (i,f,c) (cq@(CQ { cqData = mdata, cqMunicipio = mmunicipio, cqEstado = mestado, cqUltimaPos = multPos, cqAutoridade = mautoridade}),l) = 
                                            consolida' c
      where
        consolida' (NLD_Numero num) = case mdata of
            Nothing -> (cq, l)
            Just dt -> (cq { cqUltimaPos = Nothing }, (i,maybe f id multPos, makeUrnTrans num dt mmunicipio mestado mautoridade) : l)
        consolida' (NLD_Data ano mmd) = (cq { cqData = Just (ano,mmd), cqUltimaPos = nultPos},l)
        consolida' (NLD_Municipio m e) = (cq { cqAutoridade = Just $ corrigeAutoridade AC_Municipal mautoridade, cqMunicipio = Just (m,e), cqUltimaPos = nultPos },l)
        consolida' (NLD_Estado e) = (cq { cqAutoridade = Just $ corrigeAutoridade AC_Estadual mautoridade, cqEstado = Just e, cqUltimaPos = nultPos },l)
        consolida' (NLD_Autoridade a) = (cq { cqAutoridade = Just a, cqUltimaPos = nultPos}, l)
        consolida' (NLD_Esfera esf) = 
          case l of
            (i,f,u):l' -> (cq { cqUltimaPos = Nothing }, (i,f,(u . selecionaEsferaSuperior esf)):l') 
            _ -> (cq { cqUltimaPos = Nothing }, l)
        nultPos = Just $ maybe f id multPos
    makeUrnTrans num (ano,mmd) mmunicipio mestado = U.selecionaNorma'' tipoNorma num ano mmd mmunicipio mestado
    corrigeAutotidade aut1 (Just (A_Convencionada aut2)) | aut2 > aut1 = A_Convencionada aut2
    corrigeAutoridade aut _ = A_Convencionada aut
parseQualNumero ::  QualificadorParser
parseQualNumero = try $ do
  log'' "parseQualNumero: starting"
  optional $ constanteI "e"
  optional $ try abrevNumero
  (i,f,n) <- variosNumeros' numeroArabico
  log'' $ "parseQuanNumero: returning " ++ show n
  return (i,f,NLD_Numero n)

parseQualData ::  QualificadorParser
parseQualData = try $ do
  log'' "parseQualData: starting"
  optional . choice . map constanteI $ ["ambos","ambas","todos","todas"]
  optional $ constanteI "de"
  (i,f,(a,mad)) <- try (parseData >>= \ (i,f,(a,m,d)) -> return (i,f,(a,Just (m,d)))) <|>
                   (numero >>= \ (p,(n,_)) -> if 1800 <= n && 2100 >= n then return (p,p,(fromIntegral n,Nothing)) else fail "expecting ano")
  log'' $ "parseQualData: returning: a = " ++ show a ++ ", mad = " ++ show mad
  return (i,f,NLD_Data a mad)

parseQualData2 ::  QualificadorParser
parseQualData2 = try $ do
  barra
  (p,(n,_)) <- numero
  n' <- if n < 0 || (n > 99 && n < 1800) || n > 2100 then fail ("expecting ano: " ++ show n) 
        else if n < 100 then return (1900 + n)
        else return n
  return (p,p,NLD_Data (fromIntegral n') Nothing)


  

parseQualMunicipio ::  QualificadorParser
parseQualMunicipio = try $ do
    inicio <- constanteI "município"
    choice . map constanteI $ ["de","do","da"]
    (_,fim,[(municipio,estado)]) <- M.parseMunicipio
    return (inicio,fim,NLD_Municipio municipio estado)

parseQualEstado ::  QualificadorParser
parseQualEstado = try $ do
    log'' $ "parseQualEstado: starting"
    minicio <- option Nothing $ try $ do
                  minicio <- option Nothing $ fmap Just $ constanteI "do"
                  log'' $ "parseQualEstado: looking for 'estado'"
                  inicio <- constanteI "estado"
                  log'' $ "parseQualEstado: looking for 'de' or 'do' or 'da'"
                  choice . map constanteI $ ["de","do","da"]
                  return $ Just (maybe inicio id minicio)
    log'' $ "parseQualEstado: looking for nome do estado"
    (inicio2,fim,estado) <- E.parseEstado
    log'' $ "parseQualEstado: estado found: " ++ show estado
    return (maybe inicio2 id minicio,fim,NLD_Estado estado)

parseQualAutoridade ::  QualificadorParser
parseQualAutoridade = do
  try parseQualAutoridadeHifen <|> parseQualAutoridadeExtenso

parseQualAutoridadeHifen ::  QualificadorParser
parseQualAutoridadeHifen = do
    inicio <- hifen
    fim <- constanteI "cn"
    return (inicio,fim,NLD_Autoridade $ A_Normal [SJ_Instituicao (Instituicao $ Nome ["congresso","nacional"]) [] Nothing])

parseQualAutoridadeExtenso ::  QualificadorParser
parseQualAutoridadeExtenso = do
--    inicio <- constanteI "do" <|> constanteI "da"
--    mapM_ constanteI ["conselho","deliberativo","do","fundo","de","amparo","ao"]
--    fim <- constanteI "trabalhador"
--    return (inicio,fim,NLD_Autoridade $ A_Normal [SJ_Instituicao (Instituicao $ Nome ["conselho","deliberativo","fundo","amparo","trabalhador"]) [] Nothing])
    log'' $ "parseQualAutoridadeExtenso: inicio"
    (inicio,fim,nome) <- tokenListSet [
            (["do", "conselho", "deliberativo", "do", "fundo", "de", "amparo", "ao", "trabalhador"],
                  ["conselho","deliberativo","fundo","amparo","trabalhador"])
            , (["do","senado","federal"],["senado","federal"])
{-          , (["do","tst"],["tribunal","superior","trabalho"]) 
          , (["do", "tribunal","superior","do","trabalho"],["tribunal","superior","trabalho"]) 
          , (["do", "stj"],["superior","tribunal","justica"]) 
          , (["do", "superior","tribunal","de","justica"],["superior","tribunal","justica"]) 
          , (["do", "stf"],["supremo","tribunal","federal"]) 
          , (["do", "supremo","tribunal","federal"],["supremo","tribunal","federal"]) -}
      ]
    log'' $ "parseQualAutoridadeExtenso: reconhecido: " ++ concat [ w ++ " " | w <- nome]
    return (inicio,fim,NLD_Autoridade $ A_Normal [SJ_Instituicao (Instituicao $ Nome nome) [] Nothing])


type NormaFunc = [Integer] -> Int -> Maybe (Int,Int) -> URNLexML -> URNLexML

makeNormaFunc1 :: [String] -> NormaFunc
makeNormaFunc1 tipo num ano mmesdia =
  case mmesdia of
    Nothing -> U.selecionaNorma' tipo num ano
    Just (m,d) -> U.selecionaNorma tipo num d m ano

tipoNormaLei :: LinkerParserMonad (Pos,Pos,[String],[QualificadorNorma],Bool,Bool)
tipoNormaLei = do
    p <- constanteI "lei" <|> constanteI "leis"
    qual <- option [] pqualificador
    qns <- option [] pesfera
    return (p,p, ["lei"] ++ qual,qns,True, True) 
  where
    pqualificador = choice [
        (constanteI "complementar" <|> constanteI "complementares") >> return ["complementar"]
      , (constanteI "delegada" <|> constanteI "delegadas") >> return ["delegada"]
      ]

pesfera :: LinkerParserMonad [QualificadorNorma]
pesfera = choice [ (choice $ map constanteI ops) >> (return [NLD_Esfera esf]) | (ops,esf) <- opcoes ]
  where
    opcoes = [
          (["federal","federais"],AC_Federal)
        , (["estadual","estaduais"],AC_Estadual)
        , (["municipal","municipais"],AC_Municipal)
        , (["distrital","distritais"],AC_Distrital)
      ]

tipoNormaDecreto :: LinkerParserMonad (Pos,Pos,[String],[QualificadorNorma],Bool,Bool)
tipoNormaDecreto = do
    p <- constanteI "decreto" <|> constanteI "decretos"
    qual <- pqualificador
    qns <- option [] pesfera
    return (p,p,["decreto"] ++ qual,qns,True, True) 
  where
    pqualificador = choice $ map try [
          do optional hifen
             constanteI "lei" <|> constanteI "leis"
             return ["lei"]
        , return []
      ]

tipoNorma :: LinkerParserMonad (Pos,Pos,[String],[QualificadorNorma],Bool,Bool)
tipoNorma = choice [
    try tipoNormaLei,
    try $ do p <- constantesI [ "resolução","resolucao","resoluções","resolucoes"]
             return (p,p,["resolucao"],[],False,True), -- FIXME
    try tipoNormaDecreto,
    try $ do p <- constanteI "emenda"
             f <- constanteI "constitucional"
             return (p,f,["emenda","constitucional"], [],True,True), -- FIXME
{-    try $ do log'' "tentando súmula"
             p <- constanteI "súmula"
             log'' "súmula reconhecida"
             return (p,p,["sumula"],[],False,True), -}
    do inicio <- constanteI "medida" <|> constanteI "medidas"
       f <- constanteI "provisória" <|> constanteI "provisórias" <|> constanteI "provisoria" <|> constanteI "provisorias"
       return (inicio,f,["medida","provisoria"], [],True,True) -- FIXME
  ]

tokenListSet :: [([String],a)] -> LinkerParserMonad (Pos,Pos,a)
tokenListSet l = choice $ map (try . uncurry p) l
  where
    p tokens res = do
      pl <- mapM constanteI tokens
      return (head pl, last pl, res)


apelido :: LinkerParserMonad (Pos,Pos,[String],[QualificadorNorma],Bool,Bool)
apelido = do
      (p1,p2,(tipo,res,needAc,needQl)) <- tokenListSet [
            (["clt"], clt)
          , (["consolidacao","das","leis","do","trabalho"], clt)
          , (["consolidação","das","leis","do","trabalho"], clt)
        ]
      return (p1,p2,tipo,res,needAc,needQl)
  where
    clt = (["decreto","lei"],[NLD_Numero[5452], NLD_Autoridade (A_Convencionada AC_Federal), NLD_Data 1943 (Just(5,1))],False,False)

casosEspeciais :: LinkerParserMonad (Pos,Pos,[String],[QualificadorNorma],Bool,Bool)
casosEspeciais = choice $ map try [ constituicaoAbrev ]
  where
    constituicaoAbrev = do
      p1 <- constanteI "cf"
      barra
      (p2,(ano,_)) <- numero 
      return $ (p1,p2,["constituicao"],[NLD_Esfera AC_Federal, NLD_Numero [fromIntegral ano], NLD_Data (fromIntegral ano) Nothing],False,False)
    
  

numeroOuOrdinal :: LinkerParserMonad (Pos,Pos,Integer)
numeroOuOrdinal = try numeroOrdinal <|> (numero >>= \(p,(r,_)) -> return (p,p,r)) 

parseData :: LinkerParserMonad (Pos,Pos,Pos)
parseData = try parseDataExtenso <|> parseDataAbrev

parseDataExtenso :: LinkerParserMonad (Pos,Pos,Pos)
parseDataExtenso = do
  log'' $ "parseDataExtenso: starting"
  (inicio,_,dia) <- numeroOuOrdinal
  optional $ constanteI "de"
  (_,mes) <- parseMes
  optional virgula
  log'' $ "parseDataExtenso: expecting de"
  optional $ constanteI "de"
  log'' $ "parseDataExtenso: expecting year"
  (fim,(ano,_)) <- numero
  log'' $ "parseDataExtenso: returning: ano = " ++ show ano ++ ", mes = " ++ show mes ++ ", dia = " ++ show dia
  return (inicio,fim,(fromIntegral (ano :: Integer),mes,fromIntegral (dia :: Integer)))

parseDataAbrev :: LinkerParserMonad (Pos,Pos,Pos)
parseDataAbrev = do
  (inicio,(dia,_)) <- numero
  barra
  (_,(mes,_)) <- numero
  barra
  (fim,(ano,_)) <- numero
  return (inicio,fim,(fromIntegral (ano :: Integer),fromIntegral mes,fromIntegral (dia :: Integer)))
  

parseMes :: LinkerParserMonad (Pos,Int)
parseMes = casosSimples
  where
    casosSimples = do
        log'' $ "parseMes: starting"
        res <- parseLookup [("janeiro",1),("fevereiro",2),("março",3),("marco",3),
                        ("abril",4),("maio",5),("junho",6),
                        ("julho",7),("agosto",8),("setembro",9),
                        ("outubro",10),("novembro",11),("dezembro",12)]
        log'' $ "parseMes: returning res = " ++ show res
        return res


parseCases ::  [ParseCase2]
parseCases = [
      --failIfEmpty "ops2" $ parseComponente compArtigo,
      checkInitialToken >> (failIfEmpty "ops2" $ norma),
      fail "ops"
  ]   

checkInitialToken :: LinkerParserMonad ()
checkInitialToken = do
    inp <- getInput
    case inp of
      [] -> fail "empty"
      ((_,t):_) -> if f t then accept t else reject t
  where
    accept t = do
      log $ "token accepted: " ++ show t
      return ()
    reject t = do
      log $ "token rejected: " ++ show t
      anyToken
      fail "rejected"

    f (Palavra p) | (map toLower p) `S.member` initialWords = True
    f Paragrafos = True
    f Paragrafo = True
    f _ = False

initialWords = S.fromList [
    "lei",
    "leis",
    "decreto",
    "decretos",
    "resolução",
    "resolucao",    
    "resoluções",
    "resolucoes",
    "regulamento",
    "clt",
    "consolidação",
    "consolidacao",
    "constituição",
    "constituicao",
    "projeto",
    "projetos",
    "súmula",
    "sumula",
    "emenda",
    "emendas",
    "medida",
    "medidas",
    "artigo",
    "art",
    "artigos",
    "arts",
    "parágrafo",
    "paragrafo",
    "par",
    "parágrafos",
    "paragrafos",
    "pars",
    "regimento",
    "ato",
    "caput",
    "cpt",
    "inciso",
    "inc",
    "incisos",
    "incs",
    "alínea",
    "alinea",
    "alíneas",
    "alineas",
    "item",
    "itens",
    "letra",
    "letras"
  ]
