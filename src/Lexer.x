{
module Lexer where
}

%wrapper "basic"

$alpha = [_a-zA-Z]
$digit = [0-9]

tokens :-

$white+                                                 ; -- ignorar carateres brancos
"/*"([^\*]|[\r\n]|("*"+([^\*\/]|[\r\n])))*"*"+"/"       ; -- ignorar comentarios multi-linha
"int"                                                   { \_ -> TOK_INT }
"+"                                                     { \_ -> TOK_PLUS} 
"-"                                                     { \_ -> TOK_MINUS} 
"*"                                                     { \_ -> TOK_MULT}  
"/"                                                     { \_ -> TOK_DIV} 
"%"                                                     { \_ -> TOK_MOD} 
var                                                     { \_ -> TOK_VAR}
":="                                                    { \_ -> TOK_ASSIGN }
"="                                                     { \_ -> TOK_EQUAL }
"<>"                                                    { \_ -> TOK_DIFF }
"<"                                                     { \_ -> TOK_LESS }
"<="                                                    { \_ -> TOK_LESS_OR_EQUAL }
">"                                                     { \_ -> TOK_GREATER }
">="                                                    { \_ -> TOK_GREATER_OR_EQUAL }
if                                                      { \_ -> TOK_IF }
then                                                    { \_ -> TOK_THEN }
else                                                    { \_ -> TOK_ELSE }
"("                                                     { \_ -> TOK_LPAREN }
")"                                                     { \_ -> TOK_RPAREN }
";"                                                     { \_ -> TOK_SEMICOLON }
while                                                   { \_ -> TOK_WHILE }
do                                                      { \_ -> TOK_DO }
function                                                { \_ -> TOK_FUNC }
":"                                                     { \_ -> TOK_COLON }
let                                                     { \_ -> TOK_LET }
in                                                      { \_ -> TOK_IN }
end                                                     { \_ -> TOK_END }
"scani"                                                 { \_ -> TOK_SCANI }
"printi"                                                { \_ -> TOK_PRINTI }
--"&"                                                   { \_ -> TOK_AND}
--"|"                                                   { \_ -> TOK_OR}
--for                                                   { \_ -> TOK_FOR}
--to                                                    { \_ -> TOK_TO}
--break                                                 { \_ -> TOK_BREAK}
--"string"                                              { \_ -> TOK_STRING}
--"print"                                               { \_ -> TOK_PRINT }
--"["                                                   { \_ -> TOK_L_SQUARE_BRACKET}
--"]"                                                   { \_ -> TOK_R_SQUARE_BRACKET}
--of                                                    { \_ -> TOK_OF}
--","                                                   { \_ -> TOK_COMMA}
$alpha($alpha|$digit)*                                  { \s -> TOK_ID s }
$digit+                                                 { \s -> TOK_NUM (read s) }
{
data Token = TOK_ID String | TOK_NUM Int | TOK_INT | TOK_PLUS | TOK_MINUS | TOK_MULT | TOK_DIV | TOK_MOD | TOK_VAR | TOK_ASSIGN | TOK_EQUAL | TOK_DIFF | TOK_LESS | TOK_LESS_OR_EQUAL | TOK_GREATER | TOK_GREATER_OR_EQUAL | TOK_IF | TOK_THEN | TOK_ELSE | TOK_LPAREN | TOK_RPAREN | TOK_SEMICOLON | TOK_WHILE | TOK_DO | TOK_FUNC | TOK_COLON | TOK_LET | TOK_IN | TOK_END | TOK_SCANI | TOK_PRINTI deriving(Eq, Show)
}