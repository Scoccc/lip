%{
open Ast
%}

%token TRUE
%token FALSE
%token<string> IDE
%token<string> CONST
%token NOT
%token AND
%token OR
%token ADD 
%token SUB 
%token MUL 
%token EQ 
%token LEQ 
%token SKIP 
%token ASSIGN
%token SEQ 
%token IF 
%token THEN 
%token ELSE
%token WHILE
%token DO
%token LPAREN
%token RPAREN
%token LBLOCKPAREN
%token RBLOCKPAREN
%token INTEGER
%token BOOLEAN
%token EOF

%left SEQ
%nonassoc ELSE DO
%left OR AND
%nonassoc NOT
%left EQ LEQ
%left ADD SUB
%left MUL


%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | v = IDE { Var(v) }
  | n = CONST { Const(int_of_string n) }
  | NOT; e = expr { Not(e) }
  | e1 = expr; AND; e2 = expr { And(e1,e2) }
  | e1 = expr; OR; e2 = expr { Or(e1,e2) }
  | e1 = expr; ADD; e2 = expr { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1,e2) }
  | e1 = expr; EQ; e2 = expr { Eq(e1,e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1,e2) }
  | LPAREN; e = expr; RPAREN { e }
;

cmd:
 | SKIP { Skip }
 | v = IDE; ASSIGN; e = expr { Assign(v, e) }
 | c1 = cmd; SEQ; c2 = cmd { Seq(c1,c2) }
 | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e,c1,c2) }
 | WHILE; e = expr; DO; c = cmd; { While(e,c) }
 | LBLOCKPAREN; ds = decls; c = cmd; RBLOCKPAREN {Decl(ds, c)}

 decl:
  | INTEGER; v = IDE { IntVar(v) }
  | BOOLEAN; v = IDE { BoolVar(v) }

decls:
 | d = decl; SEQ; ds = decls { d :: ds }
 | { [] }

