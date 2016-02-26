module Grammar1 where

import Prelude hiding (lex)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import GrammarGenerator 

infix 4 #=

type GrammarWriter a = Writer [Prod] a

(#=) :: ProdMiddle a => a -> ProdExpr -> GrammarWriter ()
pm #= pe = tell $ [closeHead pm pe]


grammar = snd . runWriter


l s = lex $ "'" ++ s ++ "'"
lexSeq = PE_Concat . map l
lexAlt = PE_Choice . map l

linkerGrammar = grammar $ do
  "norma" #= ( nt "componenteDiretoComNome'" [nt0 "artigo"] |||
               nt "componente" [nt0 "artigo"] ) *** nt0 "norma'"
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

