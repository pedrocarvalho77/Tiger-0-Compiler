{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token  

num         { TOK_NUM $$ }
id          { TOK_ID $$ }
--string      { TOK_STRING $$ }
'int'       { TOK_TYPE_INT }
'string'    { TOK_TYPE_STRING }
'+'         { TOK_PLUS } 
'-'         { TOK_MINUS } 
'*'         { TOK_MULT }  
'/'         { TOK_DIV } 
'%'         { TOK_MOD } 
var         { TOK_VAR }
':='        { TOK_ASSIGN }
'='         { TOK_EQUAL }
'<>'        { TOK_DIFF }
'<'         { TOK_LESS }
'<='        { TOK_LESS_OR_EQUAL }
'>'         { TOK_GREATER }
'>='        { TOK_GREATER_OR_EQUAL }
if          { TOK_IF }
then        { TOK_THEN }
else        { TOK_ELSE }
'('         { TOK_LPAREN }
')'         { TOK_RPAREN }
';'         { TOK_SEMICOLON }
while       { TOK_WHILE }
do          { TOK_DO }
function    { TOK_FUNC }
':'         { TOK_COLON }
let         { TOK_LET }
in          { TOK_IN }
end         { TOK_END }
'scani'     { TOK_SCANI }
'printi'    { TOK_PRINTI }
','         { TOK_COMMA}
for         { TOK_FOR }
to          { TOK_TO }
break       { TOK_BREAK }
'print'     { TOK_PRINT }
'&'         { TOK_AND }
'|'         { TOK_OR } 
'['         { TOK_L_SQUARE_BRACKET }
']'         { TOK_R_SQUARE_BRACKET }
of          { TOK_OF }

--precedencia
%nonassoc ':='
%left '&' '|'
%nonassoc '>=' '<=' '=' '<>' '<' '>'
%left '+' '-'
%left '*' '/'
%left '%'
%nonassoc OUTERTHEN
%nonassoc else
%left NEG

%%

program : let declList in exprSeq          { LetIn $2 $4 }    

declList : decl                                   { [$1] }
         | decl declList                         {  $1 : $2 }
         
decl : varDecl                                     { VarDecl $1 }
     | funDecl                                     { FunDecl $1 }

varDecl : var id ':=' expr                         { Decl $2 $4 }

funDecl : function id '(' typeFields ')' '=' expr            { FunDef $2 $4 $7 }
        | function id '(' typeFields ')' ':' typeId '=' expr  { FunDefType $2 $4 $7 $9 }
        
typeFields : typeField                          { [$1] }
           | typeField ',' typeFields          { $1 : $3 }
           
typeField : id ':' typeId                       { TypeField $1 $3 }

expr : num                                         { Num $1 }
     | id                                          { Var $1 }
     | expr '+' expr                               { Plus $1 $3 }
     | expr '-' expr                               { Minus $1 $3 }
     | expr '*' expr                               { Times $1 $3 }
     | expr '/' expr                               { Div $1 $3 }
     | expr '%' expr                               { Mod $1 $3 }
     | expr '=' expr                               { Eq $1 $3 }
     | expr '<>' expr                              { Diff $1 $3 }
     | expr '<' expr                               { Lt $1 $3 }
     | expr '<=' expr                              { Lteq $1 $3 }
     | expr '>'  expr                              { Gt $1 $3 }
     | expr '>=' expr                              { Gteq $1 $3 }
     | expr '&' expr                               { And $1 $3 }
     | expr '|' expr                               { Or $1 $3 }
     | '-' expr %prec NEG                          { NegExp $2 }
     | id ':=' expr                                { Assign $1 $3 }
     | id '(' exprList ')'                         { FunCall $1 $3 }
     | '(' exprSeq ')'                             { ExprSeq $2 }
     | if expr then expr else expr                 { IfThenElse $2 $4 $6 }
     | if expr then expr %prec OUTERTHEN           { IfThen $2 $4 }
     | while expr do expr                          { WhileDo $2 $4 }
     | for id ':=' expr to expr do expr            { ForToDo $2 $4 $6 $8 }
     | break                                       { Break }
     | let varDeclList in exprSeq end              { LetInEnd $2 $4 }
     | 'printi' '(' expr ')'                       { PrintInt $3 }
     | 'scani' '(' ')'                             { ScanInt }
     -- | 'print' '(' string ')'                      { Print $3 }

exprSeq : expr                                     { [$1] }
        | expr ';' exprSeq                         { $1 : $3 }

exprList : expr                                    { [$1] }
         | expr ',' exprList                       { $1 : $3 }

varDeclList : varDecl                              { [$1] }
            | varDecl varDeclList                  { $1 : $2 }

typeId : 'int'                                     { TypeInt }
       | 'string'                                  { TypeString }
{

type Ident = String

data Prog = LetIn [Decl] [Exp]
    deriving Show

data Decl = VarDecl Var
          | FunDecl Fun
    deriving Show

data Var = Decl Ident Exp
    deriving Show

data Fun = FunDef Ident [TpFields] Exp
         | FunDefType Ident [TpFields] Type Exp
    deriving Show

data TpFields = TypeField Ident Type
    deriving Show

data Type = TypeInt | TypeString
    deriving (Eq, Show)

data Exp = Num Int
         | Var Ident
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | Eq Exp Exp
         | Diff Exp Exp
         | Lt Exp Exp
         | Lteq Exp Exp
         | Gt Exp Exp
         | Gteq Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | NegExp Exp
         | Assign Ident Exp
         | FunCall Ident [Exp]
         | ExprSeq [Exp]
         | IfThenElse Exp Exp Exp
         | IfThen Exp Exp
         | WhileDo Exp Exp
         | ForToDo Ident Exp Exp Exp 
         | Break
         | LetInEnd [Var] [Exp]
         | PrintInt Exp
         | ScanInt
        -- | Print String
    deriving Show

parseError :: [Token] -> a
--parseError toks = error "parse error"
parseError toks = error ("parse error" ++ show toks)
}