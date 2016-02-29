grammar LexmlLinker;


fragment DIGIT: [0-9];

fragment LARGE : [A-Z];

fragment SMALL : [a-z];

fragment ALPHA : SMALL | LARGE;

WS : [ \t\n\r\f\v]+ -> skip;


ORDINAL: DIGIT+ (INDIC_ORDINAL_FEM | INDIC_ORDINAL_MASC | 'o' | 'a');

fragment DIGITO: DIGIT;

fragment DIGITOS2: DIGIT DIGIT;

fragment DIGITOS3: DIGIT DIGIT DIGIT;

fragment DIGITOS4_MORE: DIGIT DIGIT DIGIT DIGIT+;

ANO2_MENOR_QUE_10: '0[0-9]';

ANO2_MAIOR_QUE_10: '[1-9][0-9]';

ANO4: '19' [0-9][0-9] | '20' [0-3][0-9];

NUMERO_MENOR_QUE_10: '0'* [0-9];

NUMERO_MAIOR_QUE_10: '0'* [1-9] [0-9]+;

INTEIRO_COMPOSTO: (DIGITO | DIGITOS2 | DIGITOS3) (PONTO DIGITOS3)+;

fragment NR_MAJ_C: 'C' ('C' 'C'?)?;
fragment NR_MAJ_X: 'X' ('X' 'X'?)?;
fragment NR_MAJ_I: 'I' ('I' 'I'?)?;

NUMERO_ROMANO_LARGE:
    ( 'M'+ ( 'CM' | 'D' NR_MAJ_C | 'CD' | NR_MAJ_C? ) ( 'XC' | 'L' NR_MAJ_X | 'XL' | NR_MAJ_X? ) ( 'IX' | 'V' NR_MAJ_I | 'IV' | NR_MAJ_I? ) ) |
    ( ( 'CM' | 'D' NR_MAJ_C | 'CD' | NR_MAJ_C ) ( 'XC' | 'L' NR_MAJ_X | 'XL' | NR_MAJ_X? ) ( 'IX' | 'V' NR_MAJ_I | 'IV' | NR_MAJ_I? ) ) |
    ( ( 'XC' | 'L' NR_MAJ_X | 'XL' | NR_MAJ_X ) ( 'IX' | 'V' NR_MAJ_I | 'IV' | NR_MAJ_I? ) ) |
    ( ( 'IX' | 'V' NR_MAJ_I | 'IV' | NR_MAJ_I ) ); 

fragment NR_MIN_C: 'c' ('c' 'c'?)?;
fragment NR_MIN_X: 'x' ('x' 'x'?)?;
fragment NR_MIN_I: 'i' ('i' 'i'?)?;

NUMERO_ROMANO_SMALL:
    ( 'm'+ ( 'cm' | 'd' NR_MIN_C | 'cd' | NR_MIN_C? ) ( 'xc' | 'l' NR_MIN_X | 'xl' | NR_MIN_X? ) ( 'ix' | 'v' NR_MIN_I | 'iv' | NR_MIN_I? ) ) |
    ( ( 'cm' | 'd' NR_MIN_C | 'cd' | NR_MIN_C ) ( 'xc' | 'l' NR_MIN_X | 'xl' | NR_MIN_X? ) ( 'ix' | 'v' NR_MIN_I | 'iv' | NR_MIN_I? ) ) |
    ( ( 'xc' | 'l' NR_MIN_X | 'xl' | NR_MIN_X ) ( 'ix' | 'v' NR_MIN_I | 'iv' | NR_MIN_I? ) ) |
    ( ( 'ix' | 'v' NR_MIN_I | 'iv' | NR_MIN_I ) ); 


ABREV_NUMERO: 'n.o.' | 'n.o';

LETRA: ALPHA;

PONTO: '.';

VIRGULA: ',';

PONTO_E_VIRGULA: ';';

ASPAS_DUPLAS: '"';

HIFEN: '-';

BARRA: '/';

INDIC_ORDINAL_MASC : 'º' | '°';

INDIC_ORDINAL_FEM : 'ª';

PARAGRAFO: '§';

SIMBOLO: [^A-Za-z0-9,.-/§ºª° \t\n\r\f\v];

PALAVRA_A: 'a';
PALAVRA_ADMINISTRATIVO: ( [Aa] 'dministrativo' ) | 'ADMINISTRATIVO';
PALAVRA_ALINEA: ( [Aa] 'linea' ) | 'ALINEA';
PALAVRA_ALINEAS: ( [Aa] 'lineas' ) | 'ALINEAS';
PALAVRA_AMBAS: ( [Aa] 'mbas' ) | 'AMBAS';
PALAVRA_AMBOS: ( [Aa] 'mbos' ) | 'AMBOS';
PALAVRA_AMPARO: ( [Aa] 'mparo' ) | 'AMPARO';
PALAVRA_AO: 'ao';
PALAVRA_ART: 'art';
PALAVRA_ARTIGO: ( [Aa] 'rtigo' ) | 'ARTIGO';
PALAVRA_ARTIGOS: ( [Aa] 'rtigos' ) | 'ARTIGOS';
PALAVRA_ARTS: 'arts';
PALAVRA_AS: ( [Aa] 's' ) | 'AS';
PALAVRA_ATO: ( [Aa] 'to' ) | 'ATO';
PALAVRA_BELO: ( [Bb] 'elo' ) | 'BELO';
PALAVRA_CAPUT: 'caput';
PALAVRA_CF: 'CF';
PALAVRA_CLT: 'CLT';
PALAVRA_CN: 'CN';
PALAVRA_COMPLEMENTAR: ( [Cc] 'omplementar' ) | 'COMPLEMENTAR';
PALAVRA_COMPLEMENTARES: ( [Cc] 'omplementares' ) | 'COMPLEMENTARES';
PALAVRA_CONSELHO: ( [Cc] 'onselho' ) | 'CONSELHO';
PALAVRA_CONSOLIDACAO: ( [Cc] 'onsolidacao' ) | 'CONSOLIDACAO';
PALAVRA_CONSTITUCIONAIS: ( [Cc] 'onstitucionais' ) | 'CONSTITUCIONAIS';
PALAVRA_CONSTITUCIONAL: ( [Cc] 'onstitucional' ) | 'CONSTITUCIONAL';
PALAVRA_CONSTITUICAO: ( [Cc] 'onstituicao' ) | 'CONSTITUICAO';
PALAVRA_CPT: 'cpt';
PALAVRA_DA: 'da';
PALAVRA_DAS: 'das';
PALAVRA_DE: 'de';
PALAVRA_DECRETO: ( [Dd] 'ecreto' ) | 'DECRETO';
PALAVRA_DECRETOS: ( [Dd] 'ecretos' ) | 'DECRETOS';
PALAVRA_DELEGADA: ( [Dd] 'elegada' ) | 'DELEGADA';
PALAVRA_DELEGADAS: ( [Dd] 'elegadas' ) | 'DELEGADAS';
PALAVRA_DELIBERATIVO: ( [Dd] 'eliberativo' ) | 'DELIBERATIVO';
PALAVRA_DISPOSICOES: ( [Dd] 'isposicoes' ) | 'DISPOSICOES';
PALAVRA_DISTRITAIS: ( [Dd] 'istritais' ) | 'DISTRITAIS';
PALAVRA_DISTRITAL: ( [Dd] 'istrital' ) | 'DISTRITAL';
PALAVRA_DO: 'do';
PALAVRA_DOS: 'dos';
PALAVRA_E: 'e';
PALAVRA_EMENDA: ( [Ee] 'menda' ) | 'EMENDA';
PALAVRA_ESTADO: ( [Ee] 'stado' ) | 'ESTADO';
PALAVRA_ESTADUAIS: ( [Ee] 'staduais' ) | 'ESTADUAIS';
PALAVRA_ESTADUAL: ( [Ee] 'stadual' ) | 'ESTADUAL';
PALAVRA_FEDERAIS: ( [Ff] 'ederais' ) | 'FEDERAIS';
PALAVRA_FEDERAL: ( [Ff] 'ederal' ) | 'FEDERAL';
PALAVRA_FUNDO: ( [Ff] 'undo' ) | 'FUNDO';
PALAVRA_GERAIS: ( [Gg] 'erais' ) | 'GERAIS';
PALAVRA_GOIANIA: ( [Gg] 'oiania' ) | 'GOIANIA';
PALAVRA_GOIAS: ( [Gg] 'oias' ) | 'GOIAS';
PALAVRA_HORIZONTE: ( [Hh] 'orizonte' ) | 'HORIZONTE';
PALAVRA_INC: ( [Ii] 'nc' ) | 'INC';
PALAVRA_INCISO: ( [Ii] 'nciso' ) | 'INCISO';
PALAVRA_INCISOS: ( [Ii] 'ncisos' ) | 'INCISOS';
PALAVRA_INCS: ( [Ii] 'ncs' ) | 'INCS';
PALAVRA_INTERNO: ( [Ii] 'nterno' ) | 'INTERNO';
PALAVRA_ITEM: ( [Ii] 'tem' ) | 'ITEM';
PALAVRA_ITENS: ( [Ii] 'tens' ) | 'ITENS';
PALAVRA_LEI: ( [Ll] 'ei' ) | 'LEI';
PALAVRA_LEIS: ( [Ll] 'eis' ) | 'LEIS';
PALAVRA_LETRA: ( [Ll] 'etra' ) | 'LETRA';
PALAVRA_LETRAS: ( [Ll] 'etras' ) | 'LETRAS';
PALAVRA_MEDIDA: ( [Mm] 'edida' ) | 'MEDIDA';
PALAVRA_MEDIDAS: ( [Mm] 'edidas' ) | 'MEDIDAS';
PALAVRA_MINAS: ( [Mm] 'inas' ) | 'MINAS';
PALAVRA_MUNICIPAIS: ( [Mm] 'unicipais' ) | 'MUNICIPAIS';
PALAVRA_MUNICIPAL: ( [Mm] 'unicipal' ) | 'MUNICIPAL';
PALAVRA_MUNICIPIO: ( [Mm] 'unicipio' ) | 'MUNICIPIO';
PALAVRA_NA: 'na';
PALAVRA_NAS: 'nas';
PALAVRA_NO: 'no';
PALAVRA_NOS: 'nos';
PALAVRA_NUMERO: 'numero';
PALAVRA_O: 'o';
PALAVRA_OS: ( [Oo] 's' ) | 'OS';
PALAVRA_PAR: ( [Pp] 'ar' ) | 'PAR';
PALAVRA_PARAGRAFO: ( [Pp] 'aragrafo' ) | 'PARAGRAFO';
PALAVRA_PARAGRAFOS: ( [Pp] 'aragrafos' ) | 'PARAGRAFOS';
PALAVRA_PARS: ( [Pp] 'ars' ) | 'PARS';
PALAVRA_PROVISORIA: ( [Pp] 'rovisoria' ) | 'PROVISORIA';
PALAVRA_PROVISORIAS: ( [Pp] 'rovisorias' ) | 'PROVISORIAS';
PALAVRA_REGIMENTO: ( [Rr] 'egimento' ) | 'REGIMENTO';
PALAVRA_REGULAMENTO: ( [Rr] 'egulamento' ) | 'REGULAMENTO';
PALAVRA_RESOLUCAO: ( [Rr] 'esolucao' ) | 'RESOLUCAO';
PALAVRA_RESOLUCOES: ( [Rr] 'esolucoes' ) | 'RESOLUCOES';
PALAVRA_SENADO: ( [Ss] 'enado' ) | 'SENADO';
PALAVRA_TERMOS: ( [Tt] 'ermos' ) | 'TERMOS';
PALAVRA_TODAS: ( [Tt] 'odas' ) | 'TODAS';
PALAVRA_TODOS: ( [Tt] 'odos' ) | 'TODOS';
PALAVRA_TRABALHADOR: ( [Tt] 'rabalhador' ) | 'TRABALHADOR';
PALAVRA_TRABALHO: ( [Tt] 'rabalho' ) | 'TRABALHO';
PALAVRA_TRANSITORIAS: ( [Tt] 'ransitorias' ) | 'TRANSITORIAS';
PALAVRA_ORDINAL_EXTENSO: 'unico' | 'primeiro' | 'segundo' | 'terceiro' | 'quarto' | 'quinto' | 'sexto' | 'setimo' | 'oitavo' | 'nono' | 'decimo';
PALAVRA_JANEIRO: 'janeiro';
PALAVRA_FEVEREIRO: 'fevereiro';
PALAVRA_MARCO: 'marco';
PALAVRA_ABRIL: 'abril';
PALAVRA_MAIO: 'maio';
PALAVRA_JUNHO: 'junho';
PALAVRA_JULHO: 'julho';
PALAVRA_AGOSTO: 'agosto';
PALAVRA_SETEMBRO: 'setembro';
PALAVRA_OUTUBRO: 'outubro';
PALAVRA_NOVEMBRO: 'novembro';
PALAVRA_DEZEMBRO: 'dezembro';

PALAVRA: ALPHA+;







ponto: PONTO;

indicadorOrdinalMasc: INDIC_ORDINAL_MASC | PALAVRA_O;

indicadorOrdinalFem: INDIC_ORDINAL_FEM;

virgula : VIRGULA;

pontoevirgula : PONTO_E_VIRGULA;

barra : BARRA;

hifen : HIFEN;

numero: NUMERO_MENOR_QUE_10 | NUMERO_MAIOR_QUE_10;

numeroRomano: NUMERO_ROMANO_SMALL | NUMERO_ROMANO_LARGE ;

abrevNumero: PALAVRA_NO | PALAVRA_NUMERO | ABREV_NUMERO | PALAVRA_NOS | 'nº' | 'nºs';

numeroAlfabeto_: ASPAS_DUPLAS LETRA ASPAS_DUPLAS;

numeroAlfabeto: numeroAlfabeto_ | LETRA ;

numeroArabico: ANO2_MENOR_QUE_10 | ANO2_MAIOR_QUE_10 | ANO4 | NUMERO_MENOR_QUE_10 | NUMERO_MAIOR_QUE_10 | INTEIRO_COMPOSTO;

numeroOrdinal: ORDINAL;

palavra : PALAVRA | NUMERO_ROMANO_SMALL | NUMERO_ROMANO_LARGE |
        PALAVRA_A |
        PALAVRA_ADMINISTRATIVO |
        PALAVRA_ALINEA |
        PALAVRA_ALINEAS |
        PALAVRA_AMBAS |
        PALAVRA_AMBOS |
        PALAVRA_AMPARO |
        PALAVRA_AO |
        PALAVRA_ART |
        PALAVRA_ARTIGO |
        PALAVRA_ARTIGOS |
        PALAVRA_ARTS |
        PALAVRA_AS |
        PALAVRA_ATO |
        PALAVRA_BELO |
        PALAVRA_CAPUT |
        PALAVRA_CF |
        PALAVRA_CLT |
        PALAVRA_CN |
        PALAVRA_COMPLEMENTAR |
        PALAVRA_COMPLEMENTARES |
        PALAVRA_CONSELHO |
        PALAVRA_CONSOLIDACAO |
        PALAVRA_CONSTITUCIONAIS |
        PALAVRA_CONSTITUCIONAL |
        PALAVRA_CONSTITUICAO |
        PALAVRA_CPT |
        PALAVRA_DA |
        PALAVRA_DAS |
        PALAVRA_DE |
        PALAVRA_DECRETO |
        PALAVRA_DECRETOS |
        PALAVRA_DELEGADA |
        PALAVRA_DELEGADAS |
        PALAVRA_DELIBERATIVO |
        PALAVRA_DISPOSICOES |
        PALAVRA_DISTRITAIS |
        PALAVRA_DISTRITAL |
        PALAVRA_DO |
        PALAVRA_DOS |
        PALAVRA_E |
        PALAVRA_EMENDA |
        PALAVRA_ESTADO |
        PALAVRA_ESTADUAIS |
        PALAVRA_ESTADUAL |
        PALAVRA_FEDERAIS |
        PALAVRA_FEDERAL |
        PALAVRA_FUNDO |
        PALAVRA_GERAIS |
        PALAVRA_GOIANIA |
        PALAVRA_GOIAS |
        PALAVRA_HORIZONTE |
        PALAVRA_INC |
        PALAVRA_INCISO |
        PALAVRA_INCISOS |
        PALAVRA_INCS |
        PALAVRA_INTERNO |
        PALAVRA_ITEM |
        PALAVRA_ITENS |
        PALAVRA_LEI |
        PALAVRA_LEIS |
        PALAVRA_LETRA |
        PALAVRA_LETRAS |
        PALAVRA_MEDIDA |
        PALAVRA_MEDIDAS |
        PALAVRA_MINAS |
        PALAVRA_MUNICIPAIS |
        PALAVRA_MUNICIPAL |
        PALAVRA_MUNICIPIO |
        PALAVRA_NA |
        PALAVRA_NAS |
        PALAVRA_NO |
        PALAVRA_NOS |
        PALAVRA_NUMERO |
        PALAVRA_O |
        PALAVRA_OS |
        PALAVRA_PAR |
        PALAVRA_PARAGRAFO |
        PALAVRA_PARAGRAFOS |
        PALAVRA_PARS |
        PALAVRA_PROVISORIA |
        PALAVRA_PROVISORIAS |
        PALAVRA_REGIMENTO |
        PALAVRA_REGULAMENTO |
        PALAVRA_RESOLUCAO |
        PALAVRA_RESOLUCOES |
        PALAVRA_SENADO |
        PALAVRA_TERMOS |
        PALAVRA_TODAS |
        PALAVRA_TODOS |
        PALAVRA_TRABALHADOR |
        PALAVRA_TRABALHO |
        PALAVRA_TRANSITORIAS |
        PALAVRA_ORDINAL_EXTENSO |
        PALAVRA_JANEIRO |
        PALAVRA_FEVEREIRO |
        PALAVRA_MARCO |
        PALAVRA_ABRIL |
        PALAVRA_MAIO |
        PALAVRA_JUNHO |
        PALAVRA_JULHO |
        PALAVRA_AGOSTO |
        PALAVRA_SETEMBRO |
        PALAVRA_OUTUBRO |
        PALAVRA_NOVEMBRO |
        PALAVRA_DEZEMBRO;

numeroArabicoMaiorQue10: NUMERO_MAIOR_QUE_10 | ANO2_MAIOR_QUE_10 | ANO4 | INTEIRO_COMPOSTO;          

ano2: ANO2_MENOR_QUE_10 | ANO2_MAIOR_QUE_10;

ano4: ANO4;

data: dataExtenso | dataAbrev;

dataExtenso: numeroOuOrdinal PALAVRA_DE? mes virgula? PALAVRA_DE? ano4;

dataAbrev: numero barra numero barra numero;

mes: PALAVRA_JANEIRO | PALAVRA_FEVEREIRO | PALAVRA_MARCO | PALAVRA_ABRIL | PALAVRA_MAIO | PALAVRA_JUNHO | PALAVRA_JULHO | PALAVRA_AGOSTO | PALAVRA_SETEMBRO | PALAVRA_OUTUBRO | PALAVRA_NOVEMBRO | PALAVRA_DEZEMBRO;

numeroOuOrdinal: numeroOrdinal | numero;

palavraNotE: PALAVRA | NUMERO_ROMANO_SMALL | NUMERO_ROMANO_LARGE |
        PALAVRA_A |
        PALAVRA_ADMINISTRATIVO |
        PALAVRA_ALINEA |
        PALAVRA_ALINEAS |
        PALAVRA_AMBAS |
        PALAVRA_AMBOS |
        PALAVRA_AMPARO |
        PALAVRA_AO |
        PALAVRA_ART |
        PALAVRA_ARTIGO |
        PALAVRA_ARTIGOS |
        PALAVRA_ARTS |
        PALAVRA_AS |
        PALAVRA_ATO |
        PALAVRA_BELO |
        PALAVRA_CAPUT |
        PALAVRA_CF |
        PALAVRA_CLT |
        PALAVRA_CN |
        PALAVRA_COMPLEMENTAR |
        PALAVRA_COMPLEMENTARES |
        PALAVRA_CONSELHO |
        PALAVRA_CONSOLIDACAO |
        PALAVRA_CONSTITUCIONAIS |
        PALAVRA_CONSTITUCIONAL |
        PALAVRA_CONSTITUICAO |
        PALAVRA_CPT |
        PALAVRA_DA |
        PALAVRA_DAS |
        PALAVRA_DE |
        PALAVRA_DECRETO |
        PALAVRA_DECRETOS |
        PALAVRA_DELEGADA |
        PALAVRA_DELEGADAS |
        PALAVRA_DELIBERATIVO |
        PALAVRA_DISPOSICOES |
        PALAVRA_DISTRITAIS |
        PALAVRA_DISTRITAL |
        PALAVRA_DO |
        PALAVRA_DOS |
        PALAVRA_EMENDA |
        PALAVRA_ESTADO |
        PALAVRA_ESTADUAIS |
        PALAVRA_ESTADUAL |
        PALAVRA_FEDERAIS |
        PALAVRA_FEDERAL |
        PALAVRA_FUNDO |
        PALAVRA_GERAIS |
        PALAVRA_GOIANIA |
        PALAVRA_GOIAS |
        PALAVRA_HORIZONTE |
        PALAVRA_INC |
        PALAVRA_INCISO |
        PALAVRA_INCISOS |
        PALAVRA_INCS |
        PALAVRA_INTERNO |
        PALAVRA_ITEM |
        PALAVRA_ITENS |
        PALAVRA_LEI |
        PALAVRA_LEIS |
        PALAVRA_LETRA |
        PALAVRA_LETRAS |
        PALAVRA_MEDIDA |
        PALAVRA_MEDIDAS |
        PALAVRA_MINAS |
        PALAVRA_MUNICIPAIS |
        PALAVRA_MUNICIPAL |
        PALAVRA_MUNICIPIO |
        PALAVRA_NA |
        PALAVRA_NAS |
        PALAVRA_NO |
        PALAVRA_NOS |
        PALAVRA_NUMERO |
        PALAVRA_O |
        PALAVRA_OS |
        PALAVRA_PAR |
        PALAVRA_PARAGRAFO |
        PALAVRA_PARAGRAFOS |
        PALAVRA_PARS |
        PALAVRA_PROVISORIA |
        PALAVRA_PROVISORIAS |
        PALAVRA_REGIMENTO |
        PALAVRA_REGULAMENTO |
        PALAVRA_RESOLUCAO |
        PALAVRA_RESOLUCOES |
        PALAVRA_SENADO |
        PALAVRA_TERMOS |
        PALAVRA_TODAS |
        PALAVRA_TODOS |
        PALAVRA_TRABALHADOR |
        PALAVRA_TRABALHO |
        PALAVRA_TRANSITORIAS |
        PALAVRA_ORDINAL_EXTENSO |
        PALAVRA_JANEIRO |
        PALAVRA_FEVEREIRO |
        PALAVRA_MARCO |
        PALAVRA_ABRIL |
        PALAVRA_MAIO |
        PALAVRA_JUNHO |
        PALAVRA_JULHO |
        PALAVRA_AGOSTO |
        PALAVRA_SETEMBRO |
        PALAVRA_OUTUBRO |
        PALAVRA_NOVEMBRO |
        PALAVRA_DEZEMBRO;

norma : (((compDirComNomeX1XX2XartigoX3X | componenteX2XartigoX3X))? normaX1X);
normaX1X : ((((virgula)? ((PALAVRA_TODOS | PALAVRA_TODAS | PALAVRA_AMBOS | PALAVRA_AMBAS))? (PALAVRA_DE | PALAVRA_DO | PALAVRA_DA)))? (constituicao1988 | apelidos | normaExtenso));
constituicao1988 : (PALAVRA_CONSTITUICAO (PALAVRA_FEDERAL)?);
apelidos : ((PALAVRA_REGIMENTO PALAVRA_INTERNO PALAVRA_DO PALAVRA_SENADO (PALAVRA_FEDERAL)?) | (PALAVRA_REGULAMENTO PALAVRA_ADMINISTRATIVO PALAVRA_DO PALAVRA_SENADO PALAVRA_FEDERAL) | (PALAVRA_ATO PALAVRA_DAS PALAVRA_DISPOSICOES PALAVRA_CONSTITUCIONAIS PALAVRA_TRANSITORIAS));
normaExtenso : (tipoNorma (qualificadores)?);
tipoNorma : (tnLei | tnResolucao | tnDecreto | tnEmendaConstitucional | tnMedidaProvisoria | tnCLT | tnCF);
tnLei : ((PALAVRA_LEI | PALAVRA_LEIS) ((PALAVRA_COMPLEMENTAR | PALAVRA_COMPLEMENTARES | PALAVRA_DELEGADA | PALAVRA_DELEGADAS))? ((PALAVRA_FEDERAL | PALAVRA_FEDERAIS | PALAVRA_ESTADUAL | PALAVRA_ESTADUAIS | PALAVRA_MUNICIPAL | PALAVRA_MUNICIPAIS | PALAVRA_DISTRITAL | PALAVRA_DISTRITAIS))?);
tnResolucao : (PALAVRA_RESOLUCAO | PALAVRA_RESOLUCOES);
tnDecreto : ((PALAVRA_DECRETO | PALAVRA_DECRETOS) ((hifen PALAVRA_LEI))? ((PALAVRA_FEDERAL | PALAVRA_FEDERAIS | PALAVRA_ESTADUAL | PALAVRA_ESTADUAIS | PALAVRA_MUNICIPAL | PALAVRA_MUNICIPAIS | PALAVRA_DISTRITAL | PALAVRA_DISTRITAIS))?);
tnEmendaConstitucional : (PALAVRA_EMENDA PALAVRA_CONSTITUCIONAL);
tnMedidaProvisoria : ((PALAVRA_MEDIDA | PALAVRA_MEDIDAS) (PALAVRA_PROVISORIA | PALAVRA_PROVISORIAS));
tnCLT : (PALAVRA_CLT | (PALAVRA_CONSOLIDACAO PALAVRA_DAS PALAVRA_LEIS PALAVRA_DO PALAVRA_TRABALHO));
tnCF : (PALAVRA_CF barra NUMERO);
qualificadores : ((qualificador)+ | (hifen (palavraNotE)*));
qualificador : ((((virgula | pontoevirgula))? (qualNumero | qualData | qualData2 | qualMunicipio | qualEstado)) | qualAutoridade);
qualAutoridade : (qualAutoridadeHifen | qualAutoridadeExtenso);
qualAutoridadeHifen : (hifen PALAVRA_CN);
qualAutoridadeExtenso : ((PALAVRA_DO PALAVRA_CONSELHO PALAVRA_DELIBERATIVO PALAVRA_DO PALAVRA_FUNDO PALAVRA_DE PALAVRA_AMPARO PALAVRA_AO PALAVRA_TRABALHADOR) | (PALAVRA_DO PALAVRA_SENADO PALAVRA_FEDERAL));
qualEstado : ((((PALAVRA_DO)? PALAVRA_ESTADO (PALAVRA_DE | PALAVRA_DO | PALAVRA_DA)))? estado);
estado : (PALAVRA_GOIAS | (PALAVRA_MINAS PALAVRA_GERAIS));
qualMunicipio : (PALAVRA_MUNICIPIO (PALAVRA_DE | PALAVRA_DO | PALAVRA_DA) municipio);
municipio : (PALAVRA_GOIANIA | (PALAVRA_BELO PALAVRA_HORIZONTE));
qualData : (((PALAVRA_AMBOS | PALAVRA_AMBAS | PALAVRA_TODOS | PALAVRA_TODAS))? (PALAVRA_DE)? (data | ano4));
qualData2 : (barra (ano4 | ano2));
qualNumero : ((PALAVRA_E)? (abrevNumero)? numeroArabico ((hifen (numero | numeroAlfabeto)))*);
nomesCompSingularX2XartigoX3X : (PALAVRA_ARTIGO | (PALAVRA_ART (ponto)?));
nomesCompSingularX2XparagrafoX3X : (PALAVRA_CAPUT | (PALAVRA_CPT (ponto)?) | PALAVRA_PARAGRAFO | (PALAVRA_PAR (ponto)?) | SYM_PARAGRAFO);
nomesCompSingularX2XincisoX3X : (PALAVRA_INCISO | (PALAVRA_INC (ponto)?));
nomesCompSingularX2XalineaX3X : (PALAVRA_ALINEA | PALAVRA_LETRA);
nomesCompSingularX2XitemX3X : PALAVRA_ITEM;
nomesCompPluralX2XartigoX3X : (PALAVRA_ARTIGOS | (PALAVRA_ARTS (ponto)?));
nomesCompPluralX2XparagrafoX3X : (PALAVRA_PARAGRAFOS | (PALAVRA_PARS (ponto)?) | SYM_PARAGRAFOS | (SYM_PARAGRAFO SYM_PARAGRAFO));
nomesCompPluralX2XincisoX3X : (PALAVRA_INCISOS | (PALAVRA_INCS (ponto)?));
nomesCompPluralX2XalineaX3X : (PALAVRA_ALINEAS | PALAVRA_LETRAS);
nomesCompPluralX2XitemX3X : PALAVRA_ITENS;
numerosEspecificosX2XartigoX3X : ((numeroOrdinal ((hifen PALAVRA))*) | (numeroArabicoMaiorQue10 ((hifen PALAVRA))*));
numerosEspecificosX2XparagrafoX3X : ((numeroOrdinal ((hifen (numero | numeroAlfabeto)))*) | (numeroArabicoMaiorQue10 ((hifen (numero | numeroAlfabeto)))*));
numerosEspecificosX2XincisoX3X : (numeroRomano ((hifen PALAVRA))*);
numerosEspecificosX2XalineaX3X : (numeroAlfabeto_ ((hifen PALAVRA))*);
numerosEspecificosX2XitemX3X :  {false}? ;
compDirComNomeX1XX2XartigoX3X : listaDeX2XcompDirComNomeX1X_1X2XartigoX3XX3X;
compDirComNomeX1XX2XparagrafoX3X : listaDeX2XcompDirComNomeX1X_1X2XparagrafoX3XX3X;
compDirComNomeX1XX2XincisoX3X : listaDeX2XcompDirComNomeX1X_1X2XincisoX3XX3X;
compDirComNomeX1XX2XalineaX3X : listaDeX2XcompDirComNomeX1X_1X2XalineaX3XX3X;
compDirComNomeX1X_1X2XartigoX3X : (compDirPluralX2XartigoX3X | compDirSingComNomeX2XartigoX3X);
compDirComNomeX1X_1X2XparagrafoX3X : (compDirPluralX2XparagrafoX3X | compDirSingComNomeX2XparagrafoX3X | compDirComNomeX1XX2XincisoX3X);
compDirComNomeX1X_1X2XincisoX3X : (compDirPluralX2XincisoX3X | compDirSingComNomeX2XincisoX3X);
compDirComNomeX1X_1X2XalineaX3X : (compDirPluralX2XalineaX3X | compDirSingComNomeX2XalineaX3X);
compDirPluralX2XartigoX3X : (((PALAVRA_OS | PALAVRA_NOS | PALAVRA_NAS))? ((PALAVRA_TERMOS PALAVRA_DOS))? nomesCompPluralX2XartigoX3X compDirNomeOptX1XX2XartigoX3X);
compDirPluralX2XparagrafoX3X : (((PALAVRA_OS | PALAVRA_NOS | PALAVRA_NAS))? ((PALAVRA_TERMOS PALAVRA_DOS))? nomesCompPluralX2XparagrafoX3X compDirNomeOptX1XX2XparagrafoX3X);
compDirPluralX2XincisoX3X : (((PALAVRA_OS | PALAVRA_NOS | PALAVRA_NAS))? ((PALAVRA_TERMOS PALAVRA_DOS))? nomesCompPluralX2XincisoX3X compDirNomeOptX1XX2XincisoX3X);
compDirPluralX2XalineaX3X : (((PALAVRA_AS | PALAVRA_NOS | PALAVRA_NAS))? ((PALAVRA_TERMOS PALAVRA_DAS))? nomesCompPluralX2XalineaX3X compDirNomeOptX1XX2XalineaX3X);
compDirPluralX2XitemX3X : (((PALAVRA_OS | PALAVRA_NOS | PALAVRA_NAS))? ((PALAVRA_TERMOS PALAVRA_DOS))? nomesCompPluralX2XitemX3X compDirNomeOptX1XX2XitemX3X);
compDirSingComNomeX2XartigoX3X : (compDirSingPrelimX2XartigoX3X nomesCompSingularX2XartigoX3X numerosEspecificosX2XartigoX3X (compDirSingComNome_1X2XartigoX3X)?);
compDirSingComNomeX2XparagrafoX3X : (compDirSingPrelimX2XparagrafoX3X nomesCompSingularX2XparagrafoX3X numerosEspecificosX2XparagrafoX3X (compDirSingComNome_1X2XparagrafoX3X)?);
compDirSingComNomeX2XincisoX3X : (compDirSingPrelimX2XincisoX3X nomesCompSingularX2XincisoX3X numerosEspecificosX2XincisoX3X (compDirSingComNome_1X2XincisoX3X)?);
compDirSingComNomeX2XalineaX3X : (compDirSingPrelimX2XalineaX3X nomesCompSingularX2XalineaX3X numerosEspecificosX2XalineaX3X (compDirSingComNome_1X2XalineaX3X)?);
compDirSingComNome_1X2XartigoX3X : ((virgula | pontoevirgula) compDirComNomeX1XX2XartigoX3X);
compDirSingComNome_1X2XparagrafoX3X : ((virgula | pontoevirgula) compDirComNomeX1XX2XparagrafoX3X);
compDirSingComNome_1X2XincisoX3X : ((virgula | pontoevirgula) compDirComNomeX1XX2XincisoX3X);
compDirSingComNome_1X2XalineaX3X : ((virgula | pontoevirgula) compDirComNomeX1XX2XalineaX3X);
compDirSingComNome_1X2XitemX3X : ;
compDirSingPrelimX2XartigoX3X : (PALAVRA_O | PALAVRA_NO | PALAVRA_NA | (((virgula | pontoevirgula))? PALAVRA_NOS PALAVRA_TERMOS PALAVRA_DO) | preposicaoX2XartigoX3X);
compDirSingPrelimX2XparagrafoX3X : (PALAVRA_O | PALAVRA_NO | PALAVRA_NA | (((virgula | pontoevirgula))? PALAVRA_NOS PALAVRA_TERMOS PALAVRA_DO) | preposicaoX2XparagrafoX3X);
compDirSingPrelimX2XincisoX3X : (PALAVRA_O | PALAVRA_NO | PALAVRA_NA | (((virgula | pontoevirgula))? PALAVRA_NOS PALAVRA_TERMOS PALAVRA_DO) | preposicaoX2XincisoX3X);
compDirSingPrelimX2XalineaX3X : (PALAVRA_A | PALAVRA_NO | PALAVRA_NA | (((virgula | pontoevirgula))? PALAVRA_NOS PALAVRA_TERMOS PALAVRA_DA) | preposicaoX2XalineaX3X);
compDirSingPrelimX2XitemX3X : (PALAVRA_O | PALAVRA_NO | PALAVRA_NA | (((virgula | pontoevirgula))? PALAVRA_NOS PALAVRA_TERMOS PALAVRA_DO) | preposicaoX2XitemX3X);
preposicaoX2XartigoX3X : (((virgula | pontoevirgula) PALAVRA_DO) | (PALAVRA_DO)?);
preposicaoX2XparagrafoX3X : (((virgula | pontoevirgula) PALAVRA_DO) | (PALAVRA_DO)?);
preposicaoX2XincisoX3X : (((virgula | pontoevirgula) PALAVRA_DO) | (PALAVRA_DO)?);
preposicaoX2XalineaX3X : (((virgula | pontoevirgula) PALAVRA_DA) | (PALAVRA_DA)?);
preposicaoX2XitemX3X : (((virgula | pontoevirgula) PALAVRA_DO) | (PALAVRA_DO)?);
compDirNomeOptX1XX2XartigoX3X : listaDeX2XcompDirNomeOptX1X_1X2XartigoX3XX3X;
compDirNomeOptX1XX2XparagrafoX3X : listaDeX2XcompDirNomeOptX1X_1X2XparagrafoX3XX3X;
compDirNomeOptX1XX2XincisoX3X : listaDeX2XcompDirNomeOptX1X_1X2XincisoX3XX3X;
compDirNomeOptX1XX2XalineaX3X : listaDeX2XcompDirNomeOptX1X_1X2XalineaX3XX3X;
compDirNomeOptX1XX2XitemX3X : listaDeX2XcompDirNomeOptX1X_1X2XitemX3XX3X;
compDirNomeOptX1X_1X2XartigoX3X : (compDirPluralX2XartigoX3X | compDirSingNomeOptX2XartigoX3X);
compDirNomeOptX1X_1X2XparagrafoX3X : (compDirPluralX2XparagrafoX3X | compDirSingNomeOptX2XparagrafoX3X | compDirComNomeX1XX2XincisoX3X);
compDirNomeOptX1X_1X2XincisoX3X : (compDirPluralX2XincisoX3X | compDirSingNomeOptX2XincisoX3X);
compDirNomeOptX1X_1X2XalineaX3X : (compDirPluralX2XalineaX3X | compDirSingNomeOptX2XalineaX3X);
compDirNomeOptX1X_1X2XitemX3X : (compDirPluralX2XitemX3X | compDirSingNomeOptX2XitemX3X);
compDirSingNomeOptX2XartigoX3X : (compDirSingPrelimX2XartigoX3X (nomesCompSingularX2XartigoX3X)? numerosEspecificosX2XartigoX3X (compDirSingComNome_1X2XartigoX3X)?);
compDirSingNomeOptX2XparagrafoX3X : (compDirSingPrelimX2XparagrafoX3X (nomesCompSingularX2XparagrafoX3X)? numerosEspecificosX2XparagrafoX3X (compDirSingComNome_1X2XparagrafoX3X)?);
compDirSingNomeOptX2XincisoX3X : (compDirSingPrelimX2XincisoX3X (nomesCompSingularX2XincisoX3X)? numerosEspecificosX2XincisoX3X (compDirSingComNome_1X2XincisoX3X)?);
compDirSingNomeOptX2XalineaX3X : (compDirSingPrelimX2XalineaX3X (nomesCompSingularX2XalineaX3X)? numerosEspecificosX2XalineaX3X (compDirSingComNome_1X2XalineaX3X)?);
compDirSingNomeOptX2XitemX3X : (compDirSingPrelimX2XitemX3X (nomesCompSingularX2XitemX3X)? numerosEspecificosX2XitemX3X (compDirSingComNome_1X2XitemX3X)?);
componenteX2XartigoX3X : listaDeX2XcompPlural2X2XartigoX3XX3X;
componenteX2XparagrafoX3X : listaDeX2XcompPlural2X2XparagrafoX3XX3X;
componenteX2XincisoX3X : listaDeX2XcompPlural2X2XincisoX3XX3X;
componenteX2XalineaX3X : listaDeX2XcompPlural2X2XalineaX3XX3X;
componenteX2XitemX3X : listaDeX2XcompPlural2X2XitemX3XX3X;
compPlural2X2XartigoX3X : (compPlural2_aX2XartigoX3X (compPlural2_bX2XartigoX3X)?);
compPlural2X2XparagrafoX3X : (compPlural2_aX2XparagrafoX3X (compPlural2_bX2XparagrafoX3X)?);
compPlural2X2XincisoX3X : (compPlural2_aX2XincisoX3X (compPlural2_bX2XincisoX3X)?);
compPlural2X2XalineaX3X : (compPlural2_aX2XalineaX3X (compPlural2_bX2XalineaX3X)?);
compPlural2X2XitemX3X : (compPlural2_aX2XitemX3X (compPlural2_bX2XitemX3X)?);
compPlural2_aX2XartigoX3X : (compPlural1X2XartigoX3X | compSingX2XartigoX3X);
compPlural2_aX2XparagrafoX3X : (compPlural1X2XparagrafoX3X | compSingX2XparagrafoX3X);
compPlural2_aX2XincisoX3X : (compPlural1X2XincisoX3X | compSingX2XincisoX3X);
compPlural2_aX2XalineaX3X : (compPlural1X2XalineaX3X | compSingX2XalineaX3X);
compPlural2_aX2XitemX3X : (compPlural1X2XitemX3X | compSingX2XitemX3X);
compPlural2_bX2XartigoX3X : (((((virgula compPlural2_aX2XartigoX3X))+ (virgula)?) | (((pontoevirgula compPlural2_aX2XartigoX3X))+ (pontoevirgula)?)) (compPlural2_cX2XartigoX3X)?);
compPlural2_bX2XparagrafoX3X : (((((virgula compPlural2_aX2XparagrafoX3X))+ (virgula)?) | (((pontoevirgula compPlural2_aX2XparagrafoX3X))+ (pontoevirgula)?)) (compPlural2_cX2XparagrafoX3X)?);
compPlural2_bX2XincisoX3X : (((((virgula compPlural2_aX2XincisoX3X))+ (virgula)?) | (((pontoevirgula compPlural2_aX2XincisoX3X))+ (pontoevirgula)?)) (compPlural2_cX2XincisoX3X)?);
compPlural2_bX2XalineaX3X : (((((virgula compPlural2_aX2XalineaX3X))+ (virgula)?) | (((pontoevirgula compPlural2_aX2XalineaX3X))+ (pontoevirgula)?)) (compPlural2_cX2XalineaX3X)?);
compPlural2_bX2XitemX3X : (((((virgula compPlural2_aX2XitemX3X))+ (virgula)?) | (((pontoevirgula compPlural2_aX2XitemX3X))+ (pontoevirgula)?)) (compPlural2_cX2XitemX3X)?);
compPlural2_cX2XartigoX3X : (PALAVRA_E compPlural2_aX2XartigoX3X);
compPlural2_cX2XparagrafoX3X : (PALAVRA_E compPlural2_aX2XparagrafoX3X);
compPlural2_cX2XincisoX3X : (PALAVRA_E compPlural2_aX2XincisoX3X);
compPlural2_cX2XalineaX3X : (PALAVRA_E compPlural2_aX2XalineaX3X);
compPlural2_cX2XitemX3X : (PALAVRA_E compPlural2_aX2XitemX3X);
compPlural1X2XartigoX3X : (((PALAVRA_OS | PALAVRA_NOS | PALAVRA_NAS))? nomesCompPluralX2XartigoX3X ((numeroOrdinal ((hifen (numero | numeroAlfabeto)))*) | (numeroArabico ((hifen (numero | numeroAlfabeto)))*)));
compPlural1X2XparagrafoX3X : (((PALAVRA_OS | PALAVRA_NOS | PALAVRA_NAS))? nomesCompPluralX2XparagrafoX3X ((numeroOrdinal ((hifen (numero | numeroAlfabeto)))*) | (numeroArabico ((hifen (numero | numeroAlfabeto)))*)));
compPlural1X2XincisoX3X : (((PALAVRA_OS | PALAVRA_NOS | PALAVRA_NAS))? nomesCompPluralX2XincisoX3X ((numeroArabico ((hifen PALAVRA))*) | (numeroRomano ((hifen PALAVRA))*)));
compPlural1X2XalineaX3X : (((PALAVRA_AS | PALAVRA_NOS | PALAVRA_NAS))? nomesCompPluralX2XalineaX3X ((numeroAlfabeto ((hifen PALAVRA))*) | (numeroArabico ((hifen PALAVRA))*) | (numeroRomano ((hifen PALAVRA))*)));
compPlural1X2XitemX3X : (((PALAVRA_OS | PALAVRA_NOS | PALAVRA_NAS))? nomesCompPluralX2XitemX3X numeroArabico ((hifen PALAVRA))*);
compSingX2XartigoX3X : ((componenteX2XparagrafoX3X (compSing2X2XartigoX3X)?) | compSing2X2XartigoX3X);
compSingX2XparagrafoX3X : ((componenteX2XincisoX3X (compSing2X2XparagrafoX3X)?) | compSing2X2XparagrafoX3X);
compSingX2XincisoX3X : ((componenteX2XalineaX3X (compSing2X2XincisoX3X)?) | compSing2X2XincisoX3X);
compSingX2XalineaX3X : ((componenteX2XitemX3X (compSing2X2XalineaX3X)?) | compSing2X2XalineaX3X);
compSingX2XitemX3X : compSing2X2XitemX3X;
compSing2X2XartigoX3X : (((PALAVRA_TODOS | PALAVRA_TODAS | PALAVRA_AMBOS | PALAVRA_AMBAS | (PALAVRA_NOS PALAVRA_TERMOS)))? (preposicaoX2XartigoX3X)? compSing1X2XartigoX3X);
compSing2X2XparagrafoX3X : (((PALAVRA_TODOS | PALAVRA_TODAS | PALAVRA_AMBOS | PALAVRA_AMBAS | (PALAVRA_NOS PALAVRA_TERMOS)))? (preposicaoX2XparagrafoX3X)? compSing1X2XparagrafoX3X);
compSing2X2XincisoX3X : (((PALAVRA_TODOS | PALAVRA_TODAS | PALAVRA_AMBOS | PALAVRA_AMBAS | (PALAVRA_NOS PALAVRA_TERMOS)))? (preposicaoX2XincisoX3X)? compSing1X2XincisoX3X);
compSing2X2XalineaX3X : (((PALAVRA_TODOS | PALAVRA_TODAS | PALAVRA_AMBOS | PALAVRA_AMBAS | (PALAVRA_NOS PALAVRA_TERMOS)))? (preposicaoX2XalineaX3X)? compSing1X2XalineaX3X);
compSing2X2XitemX3X : (((PALAVRA_TODOS | PALAVRA_TODAS | PALAVRA_AMBOS | PALAVRA_AMBAS | (PALAVRA_NOS PALAVRA_TERMOS)))? (preposicaoX2XitemX3X)? compSing1X2XitemX3X);
compSing1X2XartigoX3X : (((PALAVRA_O | PALAVRA_NOS | PALAVRA_NAS))? (PALAVRA_ARTIGO | (PALAVRA_ART (ponto)?)) ((numeroOrdinal ((hifen (numero | numeroAlfabeto)))*) | (numeroArabico ((hifen (numero | numeroAlfabeto)))*)));
compSing1X2XparagrafoX3X : (((PALAVRA_O | PALAVRA_NOS | PALAVRA_NAS))? (PALAVRA_CAPUT | (PALAVRA_CPT (ponto)?) | ((PALAVRA_PARAGRAFO | (PALAVRA_PAR (ponto)?) | SYM_PARAGRAFO) ((numeroOrdinal ((hifen (numero | numeroAlfabeto)))*) | (numeroArabico ((hifen (numero | numeroAlfabeto)))*)))));
compSing1X2XincisoX3X : (((PALAVRA_O | PALAVRA_NOS | PALAVRA_NAS))? (PALAVRA_INCISO | (PALAVRA_INC (ponto)?)) ((numeroArabico ((hifen PALAVRA))*) | (numeroRomano ((hifen PALAVRA))*)));
compSing1X2XalineaX3X : (((PALAVRA_A | PALAVRA_NOS | PALAVRA_NAS))? (PALAVRA_ALINEA | PALAVRA_LETRA) ((numeroAlfabeto ((hifen PALAVRA))*) | (numeroArabico ((hifen PALAVRA))*) | (numeroRomano ((hifen PALAVRA))*)));
compSing1X2XitemX3X : (((PALAVRA_O | PALAVRA_NOS | PALAVRA_NAS))? PALAVRA_ITEM numeroArabico ((hifen PALAVRA))*);
listaDeX2XcompDirComNomeX1X_1X2XartigoX3XX3X : (listaDeX1XX2XcompDirComNomeX1X_1X2XartigoX3XX4XcommaX3X | listaDeX1XX2XcompDirComNomeX1X_1X2XartigoX3XX4XsemiX3X);
listaDeX1XX2XcompDirComNomeX1X_1X2XartigoX3XX4XcommaX3X : (compDirComNomeX1X_1X2XartigoX3X ((virgula compDirComNomeX1X_1X2XartigoX3X))? (((virgula)? PALAVRA_E compDirComNomeX1X_1X2XartigoX3X))?);
listaDeX1XX2XcompDirComNomeX1X_1X2XartigoX3XX4XsemiX3X : (compDirComNomeX1X_1X2XartigoX3X ((pontoevirgula compDirComNomeX1X_1X2XartigoX3X))? (((pontoevirgula)? PALAVRA_E compDirComNomeX1X_1X2XartigoX3X))?);
listaDeX2XcompDirComNomeX1X_1X2XparagrafoX3XX3X : (listaDeX1XX2XcompDirComNomeX1X_1X2XparagrafoX3XX4XcommaX3X | listaDeX1XX2XcompDirComNomeX1X_1X2XparagrafoX3XX4XsemiX3X);
listaDeX1XX2XcompDirComNomeX1X_1X2XparagrafoX3XX4XcommaX3X : (compDirComNomeX1X_1X2XparagrafoX3X ((virgula compDirComNomeX1X_1X2XparagrafoX3X))? (((virgula)? PALAVRA_E compDirComNomeX1X_1X2XparagrafoX3X))?);
listaDeX1XX2XcompDirComNomeX1X_1X2XparagrafoX3XX4XsemiX3X : (compDirComNomeX1X_1X2XparagrafoX3X ((pontoevirgula compDirComNomeX1X_1X2XparagrafoX3X))? (((pontoevirgula)? PALAVRA_E compDirComNomeX1X_1X2XparagrafoX3X))?);
listaDeX2XcompDirComNomeX1X_1X2XincisoX3XX3X : (listaDeX1XX2XcompDirComNomeX1X_1X2XincisoX3XX4XcommaX3X | listaDeX1XX2XcompDirComNomeX1X_1X2XincisoX3XX4XsemiX3X);
listaDeX1XX2XcompDirComNomeX1X_1X2XincisoX3XX4XcommaX3X : (compDirComNomeX1X_1X2XincisoX3X ((virgula compDirComNomeX1X_1X2XincisoX3X))? (((virgula)? PALAVRA_E compDirComNomeX1X_1X2XincisoX3X))?);
listaDeX1XX2XcompDirComNomeX1X_1X2XincisoX3XX4XsemiX3X : (compDirComNomeX1X_1X2XincisoX3X ((pontoevirgula compDirComNomeX1X_1X2XincisoX3X))? (((pontoevirgula)? PALAVRA_E compDirComNomeX1X_1X2XincisoX3X))?);
listaDeX2XcompDirComNomeX1X_1X2XalineaX3XX3X : (listaDeX1XX2XcompDirComNomeX1X_1X2XalineaX3XX4XcommaX3X | listaDeX1XX2XcompDirComNomeX1X_1X2XalineaX3XX4XsemiX3X);
listaDeX1XX2XcompDirComNomeX1X_1X2XalineaX3XX4XcommaX3X : (compDirComNomeX1X_1X2XalineaX3X ((virgula compDirComNomeX1X_1X2XalineaX3X))? (((virgula)? PALAVRA_E compDirComNomeX1X_1X2XalineaX3X))?);
listaDeX1XX2XcompDirComNomeX1X_1X2XalineaX3XX4XsemiX3X : (compDirComNomeX1X_1X2XalineaX3X ((pontoevirgula compDirComNomeX1X_1X2XalineaX3X))? (((pontoevirgula)? PALAVRA_E compDirComNomeX1X_1X2XalineaX3X))?);
listaDeX2XcompDirNomeOptX1X_1X2XartigoX3XX3X : (listaDeX1XX2XcompDirNomeOptX1X_1X2XartigoX3XX4XcommaX3X | listaDeX1XX2XcompDirNomeOptX1X_1X2XartigoX3XX4XsemiX3X);
listaDeX1XX2XcompDirNomeOptX1X_1X2XartigoX3XX4XcommaX3X : (compDirNomeOptX1X_1X2XartigoX3X ((virgula compDirNomeOptX1X_1X2XartigoX3X))? (((virgula)? PALAVRA_E compDirNomeOptX1X_1X2XartigoX3X))?);
listaDeX1XX2XcompDirNomeOptX1X_1X2XartigoX3XX4XsemiX3X : (compDirNomeOptX1X_1X2XartigoX3X ((pontoevirgula compDirNomeOptX1X_1X2XartigoX3X))? (((pontoevirgula)? PALAVRA_E compDirNomeOptX1X_1X2XartigoX3X))?);
listaDeX2XcompDirNomeOptX1X_1X2XparagrafoX3XX3X : (listaDeX1XX2XcompDirNomeOptX1X_1X2XparagrafoX3XX4XcommaX3X | listaDeX1XX2XcompDirNomeOptX1X_1X2XparagrafoX3XX4XsemiX3X);
listaDeX1XX2XcompDirNomeOptX1X_1X2XparagrafoX3XX4XcommaX3X : (compDirNomeOptX1X_1X2XparagrafoX3X ((virgula compDirNomeOptX1X_1X2XparagrafoX3X))? (((virgula)? PALAVRA_E compDirNomeOptX1X_1X2XparagrafoX3X))?);
listaDeX1XX2XcompDirNomeOptX1X_1X2XparagrafoX3XX4XsemiX3X : (compDirNomeOptX1X_1X2XparagrafoX3X ((pontoevirgula compDirNomeOptX1X_1X2XparagrafoX3X))? (((pontoevirgula)? PALAVRA_E compDirNomeOptX1X_1X2XparagrafoX3X))?);
listaDeX2XcompDirNomeOptX1X_1X2XincisoX3XX3X : (listaDeX1XX2XcompDirNomeOptX1X_1X2XincisoX3XX4XcommaX3X | listaDeX1XX2XcompDirNomeOptX1X_1X2XincisoX3XX4XsemiX3X);
listaDeX1XX2XcompDirNomeOptX1X_1X2XincisoX3XX4XcommaX3X : (compDirNomeOptX1X_1X2XincisoX3X ((virgula compDirNomeOptX1X_1X2XincisoX3X))? (((virgula)? PALAVRA_E compDirNomeOptX1X_1X2XincisoX3X))?);
listaDeX1XX2XcompDirNomeOptX1X_1X2XincisoX3XX4XsemiX3X : (compDirNomeOptX1X_1X2XincisoX3X ((pontoevirgula compDirNomeOptX1X_1X2XincisoX3X))? (((pontoevirgula)? PALAVRA_E compDirNomeOptX1X_1X2XincisoX3X))?);
listaDeX2XcompDirNomeOptX1X_1X2XalineaX3XX3X : (listaDeX1XX2XcompDirNomeOptX1X_1X2XalineaX3XX4XcommaX3X | listaDeX1XX2XcompDirNomeOptX1X_1X2XalineaX3XX4XsemiX3X);
listaDeX1XX2XcompDirNomeOptX1X_1X2XalineaX3XX4XcommaX3X : (compDirNomeOptX1X_1X2XalineaX3X ((virgula compDirNomeOptX1X_1X2XalineaX3X))? (((virgula)? PALAVRA_E compDirNomeOptX1X_1X2XalineaX3X))?);
listaDeX1XX2XcompDirNomeOptX1X_1X2XalineaX3XX4XsemiX3X : (compDirNomeOptX1X_1X2XalineaX3X ((pontoevirgula compDirNomeOptX1X_1X2XalineaX3X))? (((pontoevirgula)? PALAVRA_E compDirNomeOptX1X_1X2XalineaX3X))?);
listaDeX2XcompDirNomeOptX1X_1X2XitemX3XX3X : (listaDeX1XX2XcompDirNomeOptX1X_1X2XitemX3XX4XcommaX3X | listaDeX1XX2XcompDirNomeOptX1X_1X2XitemX3XX4XsemiX3X);
listaDeX1XX2XcompDirNomeOptX1X_1X2XitemX3XX4XcommaX3X : (compDirNomeOptX1X_1X2XitemX3X ((virgula compDirNomeOptX1X_1X2XitemX3X))? (((virgula)? PALAVRA_E compDirNomeOptX1X_1X2XitemX3X))?);
listaDeX1XX2XcompDirNomeOptX1X_1X2XitemX3XX4XsemiX3X : (compDirNomeOptX1X_1X2XitemX3X ((pontoevirgula compDirNomeOptX1X_1X2XitemX3X))? (((pontoevirgula)? PALAVRA_E compDirNomeOptX1X_1X2XitemX3X))?);
listaDeX2XcompPlural2X2XartigoX3XX3X : (listaDeX1XX2XcompPlural2X2XartigoX3XX4XcommaX3X | listaDeX1XX2XcompPlural2X2XartigoX3XX4XsemiX3X);
listaDeX1XX2XcompPlural2X2XartigoX3XX4XcommaX3X : (compPlural2X2XartigoX3X ((virgula compPlural2X2XartigoX3X))? (((virgula)? PALAVRA_E compPlural2X2XartigoX3X))?);
listaDeX1XX2XcompPlural2X2XartigoX3XX4XsemiX3X : (compPlural2X2XartigoX3X ((pontoevirgula compPlural2X2XartigoX3X))? (((pontoevirgula)? PALAVRA_E compPlural2X2XartigoX3X))?);
listaDeX2XcompPlural2X2XparagrafoX3XX3X : (listaDeX1XX2XcompPlural2X2XparagrafoX3XX4XcommaX3X | listaDeX1XX2XcompPlural2X2XparagrafoX3XX4XsemiX3X);
listaDeX1XX2XcompPlural2X2XparagrafoX3XX4XcommaX3X : (compPlural2X2XparagrafoX3X ((virgula compPlural2X2XparagrafoX3X))? (((virgula)? PALAVRA_E compPlural2X2XparagrafoX3X))?);
listaDeX1XX2XcompPlural2X2XparagrafoX3XX4XsemiX3X : (compPlural2X2XparagrafoX3X ((pontoevirgula compPlural2X2XparagrafoX3X))? (((pontoevirgula)? PALAVRA_E compPlural2X2XparagrafoX3X))?);
listaDeX2XcompPlural2X2XincisoX3XX3X : (listaDeX1XX2XcompPlural2X2XincisoX3XX4XcommaX3X | listaDeX1XX2XcompPlural2X2XincisoX3XX4XsemiX3X);
listaDeX1XX2XcompPlural2X2XincisoX3XX4XcommaX3X : (compPlural2X2XincisoX3X ((virgula compPlural2X2XincisoX3X))? (((virgula)? PALAVRA_E compPlural2X2XincisoX3X))?);
listaDeX1XX2XcompPlural2X2XincisoX3XX4XsemiX3X : (compPlural2X2XincisoX3X ((pontoevirgula compPlural2X2XincisoX3X))? (((pontoevirgula)? PALAVRA_E compPlural2X2XincisoX3X))?);
listaDeX2XcompPlural2X2XalineaX3XX3X : (listaDeX1XX2XcompPlural2X2XalineaX3XX4XcommaX3X | listaDeX1XX2XcompPlural2X2XalineaX3XX4XsemiX3X);
listaDeX1XX2XcompPlural2X2XalineaX3XX4XcommaX3X : (compPlural2X2XalineaX3X ((virgula compPlural2X2XalineaX3X))? (((virgula)? PALAVRA_E compPlural2X2XalineaX3X))?);
listaDeX1XX2XcompPlural2X2XalineaX3XX4XsemiX3X : (compPlural2X2XalineaX3X ((pontoevirgula compPlural2X2XalineaX3X))? (((pontoevirgula)? PALAVRA_E compPlural2X2XalineaX3X))?);
listaDeX2XcompPlural2X2XitemX3XX3X : (listaDeX1XX2XcompPlural2X2XitemX3XX4XcommaX3X | listaDeX1XX2XcompPlural2X2XitemX3XX4XsemiX3X);
listaDeX1XX2XcompPlural2X2XitemX3XX4XcommaX3X : (compPlural2X2XitemX3X ((virgula compPlural2X2XitemX3X))? (((virgula)? PALAVRA_E compPlural2X2XitemX3X))?);
listaDeX1XX2XcompPlural2X2XitemX3XX4XsemiX3X : (compPlural2X2XitemX3X ((pontoevirgula compPlural2X2XitemX3X))? (((pontoevirgula)? PALAVRA_E compPlural2X2XitemX3X))?);
