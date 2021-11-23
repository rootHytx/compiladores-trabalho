{- 
Trabaho Compiladores,
Alexandre
Ricardo
2021/2022
-}


{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token

{- Vamos definir os tokens, ao longo do programa será assim que 
irão ser chamados -}


-- palavras reservadas
do        { DO }
let       { LET }
else      { ELSE }
end       { END }
of        { OF }
break     { BREAK }
for       { FOR }
function  { FUNCTION }
in        { IN }
then      { THEN }
var       { VAR }
if        { IF }
to        { TO }
printi    { PRINTI }
while     { WHILE }
scani     { SCANI }

-- Tipos 
int        { NUM $$ }
true       { TRUE $$ }
false      { FALSE $$ }
id         { ID $$ }
string     { STRING $$ }

-- Sinais de Pontuaçao
',' { COMMA }
':' { COLLON }
';' { SEMI_COLLON }
'(' { LPARENTH }
')' { RPARENTH }
'[' { LSQUARE_PARENTH }
']' { RSQUARE_PARENTH }

-- Operadores
'+' { PLUS }
'-' { MINUS }
'*' { MULT }
'/' { DIV }
'%' { MOD }
'=' { EQUAL }
'<>' { DIFF }
'<' { LESS }
'>' { GREATER }
'>=' { GREATERORE }
'<=' { LESSORE }
'&' { AND }
'|' { OR }
':=' { ASSIGN }


%%

{-Aqui vamos definir a gramática e 
entre chavetas o código de haskell correspondente-}

Expr : int                               { Integer $1 }
     | string                            { String $1 }
     | Expr '+' Expr                     { Op Sum $1 $3 }
     | Expr '-' Expr                     { Op Subtraction $1 $3 }
     | Expr '*' Expr                     { Op Multiplication $1 $3 } 
     | Expr '/' Expr                     { Op Division $1 $3 } 
     | Expr '%' Expr                     { Op Module $1 $3 } 
     | Expr '=' Expr                     { Op Equals $1 $3 } 
     | Expr '<>' Expr                    { Op NotEquals $1 $3 } 
     | Expr '<' Expr                     { Op Less $1 $3 } 
     | Expr '<=' Expr                    { Op LessEquals $1 $3 } 
     | Expr '>' Expr                     { Op Bigger $1 $3 } 
     | Expr '>=' Expr                    { Op BiggerEquals $1 $3 } 
     | Expr '&' Expr                     { Op And $1 $3 } 
     | Expr '|' Expr                     { Op Or $1 $3 } 
     | '-'Expr                           { Negative $2 } 
     | LValue                            { LeftValue $1 }
     | LValue ':=' Expr                  { Assign $1 $3 }
--     | id'('ExprList')'                  { FuncCall $1 $3 }
--     | '('ExprSeq')'                     { SeqExps $2 }
     | if Expr then Expr                 { If $2 $4 }
     | if Expr then Expr else Expr       { IfThen $2 $4 $6 } 
     | while Expr do Expr                { While $2 $4 }
--     | break                             { Break $1 }
--     | let VarDeclList in ExprSeq end    { LetIn $2 $4 }




LValue : id                               { $1 }

{-
ExprSeq : Expr                            { $1 }
        | ExprSeq ';' Expr

ExprList : Expr
         | ExprList ',' Expr

VarDeclList : VarDecl
            | VarDeclList VarDecl

VarDecl : var id ':=' Expr

Program: let DeclList in ExprSeq

DeclList: Decl
        | DeclList Decl

Decl: VarDecl
    | FunDecl

FunDecl: ...
       | ...

TypeFields: TypeField
          | TypeFields ',' TypeField

TypeField: id ':' TypeId

TypeId: int
      | string
-}


{
     
{- Aqui colocar o codigo de haskell, tanto os DATA que vao construir a 
árvore como o Error-}


data Expr 
        = Integer Int 
        | String String
        | Op BinaryOperator Expr Expr
        | Negative Expr
        | FuncCall String [Expr]
        | ExpSeq [Expr]
        | ScanI 
        | PrintI Expr
        | If Expr Expr
        | IfThen Expr Expr Expr
        | While Expr Expr
        | Break
        | LeftValue String
        | Assign String Expr
        deriving Show


data BinaryOperator 
        = Sum 
        | Subtraction
        | Multiplication
        | Division
        | Module
        | Equals
        | NotEquals
        | Less 
        | LessEquals
        | Bigger 
        | BiggerEquals 
        | And 
        | Or 
        deriving Show











parseError :: [Token] -> a
parseError toks = error "parse error"

}  