
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





