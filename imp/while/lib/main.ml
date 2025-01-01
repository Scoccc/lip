open Types
open Ast

let string_of_val = function
| Bool e -> string_of_bool e
| Nat e -> string_of_int e
;;

let string_of_expr = function
| _ -> failwith "todo"
;;

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let eval_expr _  = failwith "TODO"

let trace _  = failwith "TODO"
