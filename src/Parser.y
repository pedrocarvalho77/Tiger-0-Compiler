{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token  

num         { TOK_NUM $$}
id          { TOK_ID $$}
'int'       { TOK_INT }
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

--precedencia
%left '+' '-'
%left '*' '/'

%%

expr : num                                         { Num $1 }
     | lvalue                                      { $1 }
    -- | expr binary-operator expr                 {}
    -- | '-' expr                                  { (-$2) }
     | assign                                      { $1 }
    -- | id(expr-list_opt)                         {}
    -- | (expr-seq_opt)                            {}
    | if expr then expr                            { IfThen $2 $4 }
    | if expr then expr else expr                  { IfThenElse $2 $4 $6 }
    | while expr do expr                           { WhileDo $2 $4 }
    -- | let var-decl-list in                         { $2 }
    -- | let var-decl-list in expr-seq end            { () }

lvalue : id                                        { Var $1 }

assign : id ':=' expr                              { Assign $1 $3 }

--expr-seq : expr                                    { $1 }
--         | expr-seq ; expr                         { () }

--expr-list : expr                                 {}
--          | expr-list , expr                     {}

--nao sei se faz sentido isto 
--next-var-decl-list : var-decl
--                   |

--var-decl-list : var-decl                         { $1 }
--              | var-decl-list var-decl           { $2 : $1 }

--var-decl : var id := expr                        { Assign $1 $3 }

{

type Ident = String

data Exp = Num Int --deriving (Eq, Show)
         | Var Ident
         | Assign Ident Exp
         | IfThen Exp Exp
         | IfThenElse Exp Exp Exp
         | WhileDo Exp Exp
        deriving Show

-- arithmetic operators
--data BinOp = Plus
--          | Minus
--           | Times
--           | Div
--           | Mod
--           deriving (Eq, Show)        

parseError :: [Token] -> a
parseError toks = error "parse error"
}