{-# LANGUAGE FlexibleContexts #-}

module LexML.URN.Parser (parseURNLexML) where

import LexML.URN.Types
import LexML.URN.Show
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import LexML.Linker.Estados (estadosNormais)
import qualified Data.Set as S
import Control.Monad.Identity
import System.Log.Logger
import Prelude hiding (log)
import Data.Char

log = debugM "LexML.URN.Parser"

option' t = option Nothing (t >>= return . Just)

parseURNLexML :: String -> IO (Maybe URNLexML)
parseURNLexML s = do
  let s' = takeWhile (/= '@') s
  log $ "parseURNLexML: urn: " ++ s'
  case runIdentity $ runParserT pURNLexML defState "urn" s' of
    Left err -> do
      log $ "  error parsing urn: " ++ show err
      return $ Just defUrn
    Right res -> return $ Just res

defUrn = URNLexML (Local Brasil Nothing) 
               (Documento (A_Convencionada AC_Federal) 
                          (TipoDocumento1 (STD1_Norma (TipoNorma (Nome ["lei"]))) Nothing) 
                          (Descritor (TD_Datas (Datas $ Left (Data 2008 10 5)) (Just (ID_Ids [IdDocumento $ NormalID $ "123"]))) [] Nothing)) 
               Nothing 
               Nothing 
               Nothing

parseurn = test pURNLexML

test p = runIdentity . runParserT p defState "test"

data ParserState = 
  ParserState {
      psEhUmOrgao :: [Nome] -> Nome -> Bool
    , psEhUmaInstituicao :: Nome -> Bool
    , psObtemSubtipo :: Nome -> SubTipoDocumento
  } 

defState = 
  ParserState {
      psEhUmOrgao = const $ const True
    , psEhUmaInstituicao = 
        \(Nome n) -> elem n [
          ["senado","federal"],
          ["camara","deputados"],
          ["congresso","nacional"] ]
    , psObtemSubtipo = STD1_Norma . TipoNorma 
  }

ehUmOrgao :: [Nome] -> Nome -> URNParser Bool
ehUmOrgao orgaos suborgao = getState >>= \s -> return $ psEhUmOrgao s orgaos suborgao

ehUmaInstituicao :: Nome -> URNParser Bool
ehUmaInstituicao nome = getState >>= \s -> return $ psEhUmaInstituicao s nome

obtemSubTipo :: Nome -> URNParser SubTipoDocumento
obtemSubTipo nome = getState >>= \s -> return $ psObtemSubtipo s nome

type URNParser a = Parsec String ParserState a

pURNLexML :: URNParser URNLexML
pURNLexML = do
  string "urn:lex:"
  local <- pLocal
  char ':'
  doc <- pDocumento
  mversao <- option' (char '@' >> pVersao)
  mforma <- option'  (char '~' >> pForma)
  mfragmento <- option' (char '!' >> pFragmento)
  return $ URNLexML local doc mversao mforma mfragmento

pLocal :: URNParser Local
pLocal = do
  pais <- pPais
  mdetalhamento <- option' (char ';' >> pDetalhamentoLocal)
  return $ Local pais mdetalhamento

pPais :: URNParser Pais
pPais = do
  string "br"
  return Brasil

pDetalhamentoLocal :: URNParser DetalhamentoLocal
pDetalhamentoLocal = try comUF <|> pJustica Nothing
  where
    comUF = do
      uf <- pUnidadeFederacao
      try (pJustica (Just uf)) <|> pNormal uf
    pNormal uf = do
      mmunicipio <- option' (char ';' >> pMunicipio)
      return $ DLNormal uf mmunicipio
    pJustica muf =  do
      char ';'
      ramo <- pRamoJustica
      detalhes <- option [] $ do
                    char ';'
                    sepBy1 pDetalheRamo (char ';')
      return $ DLJudiciario muf (LocalJudiciario ramo detalhes)

pRamoJustica :: URNParser RamoJustica
pRamoJustica = choice [ try (string s >> return r) | 
                          (s,r) <- [(urnShow ramo, ramo) | 
                            ramo <- [ RJ_Federal, RJ_Trabalho, RJ_Eleitoral, RJ_Militar, RJ_Estadual ] ] ]

pDetalheRamo :: URNParser DetalheRamo
pDetalheRamo = choice $ map try [
      string "regiao." >> pInteger >>= return . DR_Regiao
    , string "zona." >> pInteger >>= return . DR_Zona
    , string "secao.judiciaria" >> return DR_SecaoJudiciaria
    , string "comarca" >> return DR_Comarca
    , sepBy1 pUnidadeFederacao (char ',') >>= return . DR_UnidadesFederacao
    , pMunicipio >>= return . DR_Municipio
  ]

pUnidadeFederacao :: URNParser UnidadeFederacao
pUnidadeFederacao = try $ do
    nome@(Nome nc) <- pNome
    if nc `S.member` estadosNormais then return $ UnidadeFederacao nome
            else fail $ "not a UF"

pInteger :: URNParser Integer
pInteger = many1 digit >>= return . read

pMunicipio :: URNParser Municipio
pMunicipio = pNome >>= return . Municipio                   

pNome :: URNParser Nome
pNome = sepBy1 pNomeComp (char '.') >>= return . Nome
  where
    pNomeComp = many1 alphaNum

pDocumento :: URNParser Documento
pDocumento = do
  autoridade <- pAutoridade
  char ':'
  tipoDocumento <- pTipoDocumento
  char ':'
  descritor <- pDescritor
  return $ Documento autoridade tipoDocumento descritor

pAutoridade =
  try (pAutoridadeConvencionada >>= return . A_Convencionada) <|>
      ((pSujeito `sepBy` char ',') >>= return . A_Normal)

pAutoridadeConvencionada =
  choice $ [ try (string $ urnShow s) >> return s | s <- [ 
                AC_Federal, AC_Estadual,AC_Municipal,AC_Distrital
              ] ]

pSujeito = do
    primNome <- pNome
    (char ';' >> pInstituicao primNome) <|> do
      inst <- ehUmaInstituicao primNome
      return $ if inst then SJ_Instituicao (Instituicao primNome) [] Nothing else SJ_Cargo $ Cargo $ primNome
  where
    pInstituicao inst = fmap (uncurry $ SJ_Instituicao $ Instituicao inst) $ do
      nomes <- sepBy pNome (char ';')
      case nomes of
        [] -> return ([],Nothing)
        _  -> do let (orgaos,ultimo) = splitLast nomes 
                 org <- ehUmOrgao orgaos ultimo
                 if org then return (map Orgao nomes,Nothing) 
                        else return (map Orgao orgaos, Just $ Funcao ultimo)

splitLast = splitLast' []
  where 
    splitLast' b [x] = (reverse b,x)
    splitLast' b (x:xs) = splitLast' (x:b) xs

  
pTipoDocumento = do
    subTipoOuPubOficial <- pNome
    case subTipoOuPubOficial of
      Nome ["publicacao","oficial"] -> do
        nomePeriodicoOficial <- fmap NomePeriodicoOficial pNome
        (secao,extra) <- option (Nothing,Nothing) $ do
          nsecao <- pNome
          let secao = Just $ NomeSecaoPeriodicoOficial nsecao
          option (secao,Nothing) $ do
            extra <- pDetalheExtraSuplemento
            return (secao,Just $ extra) 
        return $ TipoDocumento2 nomePeriodicoOficial secao extra

      _ -> do subTipo <- obtemSubTipo subTipoOuPubOficial
              nss <- option Nothing $ fmap Just $ do
                char ';'
                pNomeSubtipoSequenciador
              return $ TipoDocumento1 subTipo nss

pNomeSubtipoSequenciador = fmap NomeSubtipoSequenciador pNome

pDetalheExtraSuplemento = try $ do
  Nome nome <- pNome
  (td,r) <- case nome of
              ("edicao":"extra":r) -> return  (TDES_EdicaoExtra,r)
              ("suplemento":r) -> return (TDES_Suplemento,r)
              _ -> fail "expecting DetalheExtraSuplemento"
  case listToMaybe $ readsPrec 1 (concat r) of
    Just (_,(_:_)) -> fail "expecting integer"
    mnum -> return $ DetalheExtraSuplemento td (fmap fst mnum)

-- Por enquanto o parse de descritor só funcionará para urns canônicas
pDescritor = do
    datasOuAno <- pEitherDatasOuAno
    char ';'
    identificadores <- pIdentificadores
    let tipoDescritor = 
          case datasOuAno of
            Left datas -> TD_Datas datas (Just identificadores)
            Right ano -> TD_Ano ano identificadores
    componentes <- option [] (char ';' >> sepBy1 pComponenteDescritor (char ';'))    
    case componentes of
      [] -> return $ Descritor tipoDescritor [] Nothing
      _  -> case splitLast componentes of
              (comps,ComponenteDescritor (IdComponente (Nome ["retificacao",retText])) Nothing) ->
                  return $ Descritor tipoDescritor comps (Just $ read retText)
              _ -> return $ Descritor tipoDescritor componentes Nothing

pEitherDatasOuAno = (try $ fmap Left $ pDatas) <|> fmap Right pAno

pDatas = fmap Datas (fmap Left pDataSimples <|> fmap Right pDataIntervalo)

pDataSimples = do
  ano <- fmap read $ count 4 digit
  char '-'
  mes <- fmap read $ count 2 digit
  char '-'
  dia <- fmap read $ count 2 digit
  return $ Data ano mes dia

pDataIntervalo = do
  char '['
  d1 <- pDataSimples
  char ','
  d2 <- pDataSimples
  char ']'
  return (d1,d2)

pAno = fmap read $ count 4 digit

pIdentificadores = do
  ids <- many1 pIdDocumento
  case ids of
    [IdDocumento (NormalID ('l':'e':'x':'-':digits))] | isInteger digits -> 
        return $ ID_NumeroLex (read digits)
    [IdDocumento (NormalID ('s':'e':'q':'-':orgaoSeq))] -> do
        option (ID_Ids ids) $ do 
          numSeq <- subParse pSiglaNum "NumeroSeq" orgaoSeq 
          return $ ID_NumeroSeq numSeq
    _ -> return $ ID_Ids ids

pSiglaNum = do
  n <- pNome
  char '-'
  numStr <- many1 digit
  return $ NumeroSeq (SiglaOrgao n) (read numStr)

pIdDocumento = fmap IdDocumento pNormalID

pNormalID = fmap NormalID $ many1 (alphaNum <|> char '.' <|> char '_' <|> char '-')

pComponenteDescritor = do
  idc <- pIdComponente
  tit <- option Nothing $ try $ fmap Just $ do
           char ','
           pTituloComponente
  return $ ComponenteDescritor idc tit

pIdComponente = fmap IdComponente pNome

pTituloComponente = fmap TituloComponente pNome
    
pVersao = fail "not implemented"

pForma = fail "not implemented"

pFragmento = fail "not implemented"

subParse :: URNParser a -> String -> String -> URNParser a
subParse p sname t = do
    s <- getState
    let res = runIdentity $ runPT subParser s sname t 
    case res of
      Left perr -> fail $ "Error in sub parser: " ++ show perr
      Right (r,s') -> setState s' >> return r
  where
    subParser = do
      r <- p
      s <- getState
      return (r,s)

isInteger [] = False
isInteger l = and (map isDigit l)      
