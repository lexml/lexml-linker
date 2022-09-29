# LexML Linker


Parser de remissões entre normas legislativas

## Exemplos de uso

Nesta seção aprsentamos exemplos de uso do Linker, em ambiente Linux, usando a imagem docker. Os exemplos podem ser facilmente adaptados para outros ambientes, como MacOS ou Windows. 

Para tornar mais os exemplos mais sucintos, usaremos o seguinte *alias* do Bash:

```bash
alias linker='docker run -i --rm lexmlbr/lexml-linker:latest /usr/bin/linkertool'
```

Primeiramente, para obter ajuda sobre as opções de comando do linker, basta usar a opção padrão `--help`

```bash
linker --help
```

Saída:

```
Common flags:
  -f --frase=FRASE              Frase a ser analisada
     --entrada=FILE             Arquivo de entrada. Se omitido ou
                                especificado "-", então será usada a entrada
                                padrão.
  -s --saida=ITEM               Arquivo de saída. Se omitido ou especificado
                                "-", então será usada a saída padrão.
  -t --text                     Texto de entrada sem mark-up (default)
     --hxml                     Texto de entrada com mark-up (XML/HTML)                               
  -u --urns                     Gera lista de URNs reconhecidas (default)
     --html                     Gera HTML decorado com links para o resolver
                                do LexML
  -x --xml                      Gera XML decorado com as URNs reconhecidas
     --enderecoresolver[=ITEM]  Endereço do LexML Resolver, a string URNLEXML
                                será substituída pela URN. Usado quando com o
                                tipo de saída HTML
     --contexto[=URN]           URN Lex ML de contexto. Podem ser usados os
                                apelidos 'federal' e 'senado' para o contexto
                                de leis federais e resoluções do senado,
                                respectivamente, ou INLINE para ser lido
                                alternadamente às linhas de entrada
     --logaregras               Loga a execução das regras
     --logatokens               Loga a saída da análise léxica
     --calculamd5               Calcula o MD5 da entrada
  -d --debug                    Mostra mensagens de debug
  -? --help                     Display help message
  -V --version                  Print version information
```

Os parâmetros do linker estão nas seguintes categorias:

| Categoria | Parâmetro | Descrição | Exemplos |
|----|----|----|----|
| Fonte de Entrada | `-f` {texto}, `--frase=`{texto} | texto de entrada especificado diretamente na linha de comando | `linker -f 'Os incisos I e III do par. 3o do art. 8o da Lei n.o. 12.527, de 18 de novembro de 2011'` |
|     | `--entrada=`FILE | o arquivo no caminho `FILE` é usado como entrada. Porém, ao usar o linker via container, o caminho precisa ser um caminho de dentro do container, o que torna essa opção pouco útil. Mas a mesma função pode ser obtida através do uso da entrada padrão como fonte. |  |
|     | `--entrada=-` | o texto é obtido à partir da entrada padrão do programa. Esse é o comportamento padrão caso nenhuma opção de fonte de entrada seja especificada. | `echo 'Os incisos I e III do par. 3o do art. 8o da Lei n.o. 12.527, de 18 de novembro de 2011' \| linker` |
|Formato da entrada|`-t`, `--text`|A entrada é um aquivo texto simples. Este é o formato de entrada padrão caso nenhuma opção de formato de entrada seja especificada.| `echo 'Os incisos I e III do par. 3o do art. 8o da Lei n.o. 12.527, de 18 de novembro de 2011' \| linker` |
|  |`--hxml`|A entrada é um aquivo XML (formato LexML) ou X/HTML. | `echo '<p>Os incisos I e III do par. 3o do art. 8o da Lei n.o. 12.527, de 18 de novembro de 2011</p>' \| linker --hxml` |
| Formato de Saída | `-u`, `--urns` | Gera na saída a lista de URNs de referências encontradas no texto. Este é o formato de saída padrão caso nenhuma opção de formato de saída seja especificada. | `echo 'Os incisos I e III do par. 3o do art. 8o da Lei n.o. 12.527, de 18 de novembro de 2011' \| linker` |
|    | `--html` | Gera na saída o HTML de entrada decorado com hiperlinks para o site LexML. O endereço base para as URLs é dado pela opção --enderecoresolver`. | `echo '<p>Os incisos I e III do par. 3o do art. 8o da Lei n.o. 12.527, de 18 de novembro de 2011</p>' \| linker --hxml --html` |
|    | `-x`, `--xml` | Saída no formato XML do LexML com links anotados com URNs. | `echo '<p>Os incisos I e III do par. 3o do art. 8o da Lei n.o. 12.527, de 18 de novembro de 2011</p>' \| linker --hxml --xml` |
|    |  --enderecoresolver={URL template} | Especifica o template para construir os URLs na opção `--html`. O template deve ser uma URL contendo a string  `URNLEXML`, que será substituida pela URN identificada. | ``echo '<p>Os incisos I e III do par. 3o do art. 8o da Lei n.o. 12.527, de 18 de novembro de 2011</p>' \| linker --hxml --html --enderecoresolver="https://normas.leg.br/?urn=URNLEXML"``  |
