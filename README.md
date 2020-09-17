# Analisador JSON

**Disciplina**: FGA0210 - PARADIGMAS DE PROGRAMAÇÃO - T01 <br>
**Nro do Grupo**: 04<br>
**Paradigma**: Funcional<br>

## Alunos
|Matrícula | Aluno |
| -- | -- |
| 17/0056155  |  André Freitas |
| 17/0013693  |  João Gabriel Rossi |
| 17/0113060  |  Pedro Vítor de Salles Cella |
| 17/0045269  |  Sara Campos |
 
## Sobre 
O projeto se trata da criação de um Parser de JSON utilizando da linguagem Haskell. 

## Screenshots
Adicione 2 ou mais screenshots do projeto em termos de interface e/ou funcionamento.

## Instalação 
**Linguagens**: Haskell<br>
**Tecnologias**: Stack<br>
O usuario deve entrar no ghci para mais facil interacao com o programa, escrevendo o seguinte comando:  
`ghci app/Main.hs -i src/Json.hs`

## Uso 
Apos entrar no ghci, o usuario pode escolher 2 opcoes:
- Entrar com o Json manualmente, utilizando o comando: `run "JSON_DO_USUARIO"` se for uma string, necessita de `\"`  
  ex: `run "{ \"key\" : \"value\" }"`;
- Entrar com um arquivo, utilizando o comando: `readAndParseFile "path_to_file.json" jsonParser`;

## Vídeo
Adicione 1 ou mais vídeos com a execução do projeto.

## Outros 
Quaisquer outras informações sobre seu projeto podem ser descritas a seguir.

## Fontes
*TSODING, **JSON Parser 100% From Scratch in Haskell (only 111 lines)**. Youtube.  
Disponivel em: <https://www.youtube.com/watch?v=N9RUqGYuGfw>*
