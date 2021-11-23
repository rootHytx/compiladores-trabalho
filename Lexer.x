{
module Lexer where
}

%wrapper "basic"

--Expressoes regulares
$digit =  [0-9]
$white = [\ \t\n\r\s]
$alpha = [_a-zA-Z]

tokens :-

$white+  ; --ignorar carateres brancos

-- Palavras reservadas
"var"       { \_ -> VAR }
"break"     { \_ -> BREAK }
"do"        { \_ -> DO }
"for"       { \_ -> FOR }
"if"        { \_ -> IF }
"in"        { \_ -> IN }
"of"        { \_ -> OF }
"to"        { \_ -> TO }
"let"       { \_ -> LET }
"then"      { \_ -> THEN }
"else"      { \_ -> ELSE }
"while"     { \_ -> WHILE }
"scani"     { \_ -> SCANI }
"printi"    { \_ -> PRINTI }
"function"  { \_ -> FUNCTION }
"end"       { \_ -> END }

--sinais de pontuaçao
","         { \_ -> COMMA }
":"         { \_ -> COLLON }
";"         { \_ -> SEMI_COLLON }
"("         { \_ -> LPARENTH}
")"         { \_ -> RPARENTH}
"["         { \_ -> LSQUARE_PARENTH}
"]"         { \_ -> RSQUARE_PARENTH}

-- comentarios
"//".* ;
"/*"(\s|\n|.)*"*/" ;

-- tipos 
--DUVIDA PERGUNTAR AO STOR PARA SABER SE ESTÁ BEM DEFINIDO
$digit+                     { \d -> NUM (read d) }
\".*\"                      { \s -> STRING (read s)}
true                        { \_ -> TRUE True }
false                       { \_ -> FALSE False }
$alpha($alpha|$digit|"_")*  { \d -> ID d }

-- operadores
"+"         { \_ -> PLUS }
"-"         { \_ -> MINUS }
"*"         { \_ -> MULT }
"/"         { \_ -> DIV }
"%"         { \_ -> MOD }
"="         { \_ -> EQUAL }
"<>"        { \_ -> DIFF }
"<"         { \_ -> LESS }
"<="        { \_ -> LESSORE }
">"         { \_ -> GREATER }
">="        { \_ -> GREATERORE }
"&"         { \_ -> AND }
"|"         { \_ -> OR }
":="        { \_ -> ASSIGN }

{

data Token
  = PLUS                 --operadores
  | MINUS
  | MULT
  | DIV
  | MOD
  | EQUAL
  | DIFF
  | LESS
  | LESSORE
  | GREATER
  | GREATERORE
  | AND
  | OR 
  | ASSIGN              --fim dos operadores
  | VAR                 --palavras reservadas
  | BREAK 
  | DO 
  | FOR 
  | IF
  | IN 
  | OF 
  | TO
  | LET 
  | THEN 
  | ELSE 
  | WHILE 
  | SCANI
  | PRINTI
  | FUNCTION 
  | END                 --fim das palavras reservadas
  | COMMA               --sinais de pontuação
  | COLLON
  | SEMI_COLLON
  | LPARENTH 
  | RPARENTH
  | LSQUARE_PARENTH 
  | RSQUARE_PARENTH     --fim dos sinais de pontuaçao
  | NUM Int             --tipos
  | ID String 
  | STRING String 
  | TRUE Bool
  | FALSE Bool

  deriving (Eq, Show)
}
