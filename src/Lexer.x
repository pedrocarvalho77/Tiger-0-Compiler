{
module Lexer where
}

%wrapper "basic"

$alpha = [_a-zA-Z]
$digit = [0-9]

tokens :-

$white+                                                 ; -- ignorar carateres brancos
"/*"([^\*]|[\r\n]|("*"+([^\*\/]|[\r\n])))*"*"+"/"       ; -- ignorar comentarios multi-linha

"int"                                                   { \_ -> INT }
"+"                                                     { \_ -> PLUS} 
"-"                                                     { \_ -> MINUS} 
"*"                                                     { \_ -> MULT}  
"/"                                                     { \_ -> DIV} 
"%"                                                     { \_ -> MOD} 
var                                                     { \_ -> VAR}
":="                                                    { \_ -> ASSIGN }
"="                                                     { \_ -> EQUAL }
"<>"                                                    { \_ -> DIFF }
"<"                                                     { \_ -> LESS }
"<="                                                    { \_ -> LESS_OR_EQUAL }
">"                                                     { \_ -> GREATER }
">="                                                    { \_ -> GREATER_OR_EQUAL }
if                                                      { \_ -> IF }
then                                                    { \_ -> THEN }
else                                                    { \_ -> ELSE }
"("                                                     { \_ -> LPAREN }
")"                                                     { \_ -> RPAREN }
";"                                                     { \_ -> SEMICOLON }
while                                                   { \_ -> WHILE }
do                                                      { \_ -> DO }
end                                                     { \_ -> END }
function                                                { \_ -> FUNC }
":"                                                     { \_ -> COLON}
let                                                     { \_ -> LET}
in                                                      { \_ -> IN}
"scani"                                                 { \_ -> SCANI }
"printi"                                                { \_ -> PRINTI }

--"&"                                                   { \_ -> AND}
--"|"                                                   { \_ -> OR}
--for                                                   { \_ -> FOR}
--to                                                    { \_ -> TO}
--break                                                 { \_ -> BREAK}
--"string"                                              { \_ -> STRING}
--"print"                                               { \_ -> PRINT }

--"["                                                   { \_ -> L_SQUARE_BRACKET}
--"]"                                                   { \_ -> R_SQUARE_BRACKET}
--of                                                    { \_ -> OF}
--","                                                   { \_ -> COMMA}

$alpha($alpha|$digit)*                                  { \s -> ID s }
$digit+                                                 { \s -> NUM (read s) }


{
data Token = ID String | NUM Int | INT | PLUS | MINUS | MULT | DIV | MOD | VAR |ASSIGN | EQUAL | DIFF | LESS | LESS_OR_EQUAL | GREATER | GREATER_OR_EQUAL | IF | THEN | ELSE | LPAREN | RPAREN | SEMICOLON | WHILE | DO | END | FUNC | COLON |LET | IN |SCANI | PRINTI deriving(Eq, Show)
}