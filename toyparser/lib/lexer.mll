{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex = ("0x"|"0X")['0'-'9' 'a'-'f' 'A'-'F']*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIV }
  | "*" { TIMES }
  | num { CONST (Lexing.lexeme lexbuf) }
  | hex { HEX (Lexing.lexeme lexbuf) }
  | eof { EOF }
