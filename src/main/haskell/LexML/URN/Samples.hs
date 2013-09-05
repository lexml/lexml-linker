module LexML.URN.Samples where

import LexML.URN.Types
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Random
import Data.List

allSamples :: Samples a => Int -> [a]
allSamples = fst . samples . mkStdGen

class Samples a where
  samples :: StdGen -> ([a],StdGen)

samples' :: Samples a => StdGen -> [a]
samples' = fst . samples

maybeSamples :: Samples a => StdGen -> ([Maybe a],StdGen)
maybeSamples g = let (r,g') = samples g in (Nothing : map Just r,g')

instance Samples a => Samples (Maybe a) where
  samples = maybeSamples

pairSamples :: (Samples a, Samples b) => StdGen -> ([(a,b)],StdGen)
pairSamples g = ([(a,b) | a <- s1, b <- s2],g2)
  where
    (s1,g1) = samples g
    (s2,g2) = samples g1

instance (Samples a, Samples b) => Samples (a,b) where
  samples g = ([(a,b) | a <- samples' g1, b <- samples' g2],g')
    where (g',[g1,g2]) = splitMany g 2

instance (Samples a, Samples b, Samples c) => Samples (a,b,c) where
  samples g = ([(a,b,c) | (a,(b,c)) <- samples' g1],g')
    where
      (g',g1) = split g

instance (Samples a, Samples b, Samples c, Samples d) => Samples (a,b,c,d) where
  samples g = ([(a,b,c,d) | (a,(b,(c,d))) <- samples' g1],g')
    where
      (g',g1) = split g

instance (Samples a, Samples b, Samples c, Samples d, Samples e) => Samples (a,b,c,d,e) where
  samples g = ([(a,b,c,d,e) | (a,(b,(c,(d,e)))) <- samples' g1],g')
    where
      (g',g1) = split g

instance (Samples a, Samples b) => Samples (Either a b) where
  samples g = ([Left s | s <- samples' g1] ++ [Right s | s <- samples' g2],g')
    where (g',[g1,g2]) = splitMany g 2

samplesList g = ([] : res,g3)
  where
    (s1,g1) = samples g
    maxElems = length s1
    numLists = if maxElems > 1 then 3 else 1
    (rl1,g2:_) = unzip $ take numLists $ iterate (next . snd) (next g1)    
    sizes = [ (n `mod` maxElems) + 1 | n <- rl1 ]
    (g3,res) = mapAccumR (select s1) g2 sizes
    select l g 0 = (g,[])
    select l g len = (g2,x:l')
      where
        (n,g1) = next g
        (bef,x:aft) = splitAt (n `mod` len) l
        (g2,l') = select (bef ++ aft) g1 (len-1)

instance Samples a => Samples [a] where
  samples = samplesList

splitMany g n = mapAccumR (\g _ -> split g) g [1 .. n]

class Convertible a b | a -> b where
  convert :: b -> a

instance (Convertible a b, Samples b) => Samples a where
  samples g = let (l,g') = samples g in (map convert l,g')

instance Convertible URNLexML (Local,Documento,Maybe Versao,Maybe Forma,Maybe Fragmento) where
  convert (a,b,c,d,e) = URNLexML a b c d e

instance Convertible Local (Pais,Maybe DetalhamentoLocal) where
  convert (a,b) = Local a b

instance Samples Data where
  samples g = ([ Data ano mes dia | ano <- [1970,1998], mes <- [1,5], dia <- [4] ],g)

instance Samples Datas where
  samples g = let (r,g1) = samples g in ([Datas d | d <- r],g1)     

instance Samples Pais where
  samples g = ([Brasil],g)

instance Samples DetalhamentoLocal where
  samples g = ([DLNormal uf mun | (uf,mun) <- samples' g1] ++ 
               [DLJudiciario uf lj | (uf,lj) <- samples' g2],g')
    where
      (g',[g1,g2]) = splitMany g 2
{-
instance Samples LocalJudiciario where
  samples g = ([LocalJudiciario rj dr | rj <- samples' g1, dr <- samples' g2],g')
    where
      (g',[g1,g2]) = splitMany g 2
-}

instance Convertible LocalJudiciario (RamoJustica, [DetalheRamo]) where
  convert (r,d) = LocalJudiciario r d

instance Samples RamoJustica where
  samples g = ([RJ_Federal, RJ_Trabalho, RJ_Eleitoral, RJ_Militar, RJ_Estadual],g)

instance Samples UnidadeFederacao where
  samples g = (map (UnidadeFederacao . Nome) unidades,g) 
    where
      unidades = [ ["distrito","federal"],["sao","paulo"],["rio","de","janeiro"] ]

instance Samples Municipio where
  samples g = (map (Municipio . Nome) nomes,g) 
    where
      nomes = [ ["brasilia"],["sao","carlos"],["pindamonhagaba"] ]

instance Samples Instituicao where
  samples g = (map (Instituicao . Nome) nomes,g) 
    where
      nomes = [ ["senado", "federal"],["supremo", "tribunal", "federal"],["camara", "legislativa"] ]

instance Samples DetalheRamo where
  samples g = ([DR_Regiao 1, DR_Zona 2, DR_SecaoJudiciaria, DR_Comarca] ++ 
               [DR_UnidadesFederacao uf | uf <- samples' g1] ++ [DR_Municipio m | m <- samples' g2],g')
    where
      (g',[g1,g2]) = splitMany g 2

instance Convertible Documento (Autoridade,TipoDocumento,Descritor) where
  convert (aut,td,desc) = Documento aut td desc

instance Samples Autoridade where
  samples g = ([A_Normal sl | sl <- samples' g1] ++ [A_Convencionada ac | ac <- samples' g2],g')
    where
      (g',[g1,g2]) = splitMany g 2

instance Samples AutoridadeConvencionada where
  samples g = ([AC_Federal,AC_Estadual,AC_Municipal,AC_Distrital],g)

instance Samples Sujeito where
  samples g = ([SJ_Instituicao inst orgs f | (inst,orgs,f) <- samples' g1] ++
               [SJ_Cargo cargo | cargo <- samples' g2], g')
    where
      (g',[g1,g2]) = splitMany g 2

instance Samples Nome where
  samples g = (map Nome [["meu","nome"],["outro","nome"]],g)      

instance Samples Orgao where
  samples = fromNomes Orgao [ ["secretaria"],["controladoria"],["chefia", "de", "gabinete"] ]

instance Samples Funcao where
  samples = fromNomes Funcao [ ["funcao1"],["funcao2"] ]

instance Samples Cargo where
  samples = fromNomes Cargo [ ["cargo1"],["cargo2"] ]

instance Samples TipoDocumento where
  samples g = ([TipoDocumento1 s n | (s,n) <- samples' g1] ++ 
               [TipoDocumento2 npo nspo des | (npo,nspo,des) <- samples' g2],g')
    where
      (g',[g1,g2]) = splitMany g 2

instance Samples SubTipoDocumento where
  samples g = ([STD1_Norma tn | tn <- samples' g1] ++
               [STD1_Jurisprudencia tj | tj <- samples' g2] ++
               [STD1_ProjetoNorma tpn | tpn <- samples' g3],g')
    where
      (g',[g1,g2,g3]) = splitMany g 3

instance Samples Integer where
  samples g = ([1, 5, 19, 357, 1256],g)

instance Samples DetalheExtraSuplemento where
  samples g = ([DetalheExtraSuplemento tdes n | (tdes,n) <- samples' g1],g')
    where
      (g1,g') = split g

instance Samples TipoDetalheExtraSuplemento where
  samples g = ([TDES_EdicaoExtra, TDES_Suplemento],g)      

instance Samples Descritor where
  samples g = ([Descritor td cd sr | (td,cd,sr) <- samples' g1],g')
    where (g1,g') = split g

instance Samples TipoDescritor where
  samples g = ([TD_Datas d ids | (d,ids) <- samples' g1] ++
               [TD_Ano 2007 ids | ids <- samples' g2] ++
               [TD_Apelido doa ad | (doa,ad) <- samples' g3],g')
    where
      (g',[g1,g2,g3]) = splitMany g 3


instance Samples DatasOuAno where
  samples g = (map DatasOuAno $ [Left d | d <- samples' g1] ++ [Right 2007],g')
    where 
      (g',g1) = split g

instance Samples ComponenteDescritor where
  samples g = ([ComponenteDescritor id tc | (id,tc) <- samples' g1],g')
    where
      (g',g1) = split g

fromNomes :: (Nome -> a) -> [[String]] -> StdGen -> ([a],StdGen)
fromNomes c l g = (map (c . Nome) l,g)

instance Samples TipoProjetoNorma where
  samples = fromNomes TipoProjetoNorma [["projeto","lei"],["projeto","emenda","constitucional"]]

instance Samples TipoNorma where
  samples = fromNomes TipoNorma [["lei"],["lei","complementar"],["decreto"],["emenda","constitucional"]]
  

instance Samples TipoJurisprudencia where
  samples = fromNomes TipoJurisprudencia [["supremo","tribunal","federal"],["superior","tribunal","justica"],["tribunal","superior","trabalho"]]

instance Samples NomeSubtipoSequenciador where
  samples = fromNomes NomeSubtipoSequenciador [["nss1"]]

instance Samples NomePeriodicoOficial where
  samples = fromNomes NomePeriodicoOficial [["diario","justica"],["diario","oficial","uniao"]]

instance Samples NomeSecaoPeriodicoOficial where
  samples = fromNomes NomeSecaoPeriodicoOficial [["revista","jurisprudencia","superior","tribunal","justica"],["boletim","administrativo","pessoal"]]

instance Samples ApelidoDocumento where
  samples = fromNomes ApelidoDocumento [["codigo","penal"],["codigo","civil"]]

instance Samples IdComponente where
  samples = fromNomes IdComponente [["1234"],["12"],["456"]]

instance Samples TituloComponente where
  samples = fromNomes TituloComponente [["titulo","componente"]]

instance Samples SiglaOrgao where
  samples = fromNomes SiglaOrgao [["sf"],["cd"],["stf"]]

instance Samples Evento where
  samples = fromNomes Evento [["publicacao"],["revogacao"],["republicacao"]]

instance Samples TipoForma where
  samples = fromNomes TipoForma [["pdf"],["html"],["rtf"]]


instance Samples IdDocumento where
  samples g = let (l,g') = samples g in (map IdDocumento l,g')

instance Samples Fragmento where
  samples g = let (l,g') = samples g in (map Fragmento l,g')

instance Samples Visao where
  samples g = let (l,g') = samples g in (map Visao l,g')

instance Samples NormalID where
  samples g = (map NormalID ["normal.id_" ++ show n | n <- [1..2]],g)

instance Samples LinguaCodigo where
  samples g = (map LinguaCodigo ["br","en","pt"],g)

instance Samples LinguaScript where
  samples g = (map LinguaScript ["script1","script2"],g)

instance Samples LinguaRegiao where
  samples g = (map LinguaRegiao ["regiao1","regiao2"],g)

instance Samples Identificadores where
  samples g = ([ID_Ids ids | ids <- samples' g1] ++
               [ID_NumeroLex 12] ++
               [ID_NumeroSeq ns | ns <- samples' g2],g)
    where
      (g',[g1,g2]) = splitMany g 2

instance Samples NumeroSeq where
  samples g = ([NumeroSeq so 12 | so <- samples' g1],g')
    where
      (g1,g') = split g

instance Samples CompFragmento where
  samples g = ([CompFragmento tcf it | (tcf,it) <- samples' g1],g')
    where
      (g1,g') = split g

instance Samples Versao where
  samples g = ([Versao tv dv | (tv,dv) <- samples' g1],g')
    where
      (g1,g') = split g
            
instance Samples TipoComponenteFragmento where
  samples g = ([TCF_Parte,TCF_Livro,TCF_Titulo
               ,TCF_Capitulo,TCF_Secao,TCF_SubSecao
               ,TCF_AgrupamentoHierarquico,TCF_Artigo
               ,TCF_Caput,TCF_Paragrafo,TCF_Inciso
               ,TCF_Alinea,TCF_Item,TCF_DispositivoGenerico
               ,TCF_Aspas],g)

instance Samples TipoVersao where
  samples g = ([TV_Datas dt | dt <- samples' g1] ++ [TV_VersaoOriginal,TV_InicioVigencia,TV_MultiVigente] ++
               [TV_VersaoVigenteEm dt | dt <- samples' g2] ++ [TV_VersaoEficazEm dt | dt <- samples' g3] ++
               [TV_VersaoConsultadaEm dt | dt <- samples' g4],g')
    where
      (g',[g1,g2,g3,g4]) = splitMany g 4 

instance Samples DetalheVersao where
  samples g = ([DetalheVersao ev v | (ev,v) <- samples' g1],g')
    where (g1,g') = split g

instance Samples Forma where
  samples g = ([Forma tf l | (tf,l) <- samples' g1],g')
    where (g1,g') = split g

instance Samples Lingua where
  samples g = ([Lingua lc ls lr | (lc,ls,lr) <- samples' g1],g')
    where (g1,g') = split g
