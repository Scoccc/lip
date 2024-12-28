%{
open Ast
%}

%token <string> CONST
%token <string> HEX
%token PLUS
%token MINUS
%token DIV
%token TIMES
%token LPAREN
%token RPAREN
%token EOF

%left MINUS PLUS
%left DIV TIMES
%nonassoc UMINUS

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | n = HEX { Hex(int_of_string n) }
  | MINUS; e = expr %prec UMINUS { Neg e }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | e1 = expr; TIMES; e2 = expr { Mul(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;
