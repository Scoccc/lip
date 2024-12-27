{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let vowel = ['a' 'e' 'i' 'o' 'u']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let digit = ['0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let atok = ['A'-'Z'] chr*
let btok = vowel+
let ctok = (letter # vowel)* vowel? (letter # vowel)* 
let dtok = '-'? digit* '.'? digit*
let etok = ("0x"|"0X")['0'-'9' 'A'-'F' 'a'-'f']+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | atok { ATOK (Lexing.lexeme lexbuf) }
  | btok { BTOK (Lexing.lexeme lexbuf) }
  | ctok { CTOK (Lexing.lexeme lexbuf) }
  | dtok { DTOK (Lexing.lexeme lexbuf) }
  | etok { ETOK (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
