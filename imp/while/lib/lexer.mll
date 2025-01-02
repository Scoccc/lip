{
open Parser
}

let white = [' ' '\t' '\n' '\r']+
let id = ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '_']*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | "skip" { SKIP }
  | ":=" { ASSIGN }
  | ";" { SEQ }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | id { VAR(Lexing.lexeme lexbuf) }
  | num { CONST(Lexing.lexeme lexbuf) }
  | eof { EOF }