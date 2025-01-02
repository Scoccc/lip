open Types
open Ast

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let rec eval_expr (st : state) (e : expr) : memval =
  match e with
  | True -> Bool true
  | False -> Bool false
  | Var x -> 
    let env = topenv st in
    (match env x with
    | IVar loc -> getmem st loc
    | BVar loc -> getmem st loc
    | exception UnboundVar _ -> raise (TypeError ("Unbound variable: " ^ x)))
  | Const n -> Int n
  | Not e1 -> (
      match eval_expr st e1 with
      | Bool b -> Bool (not b)
      | _ -> failwith "Type error: Not expects a boolean"
    )
  | And (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Bool b1, Bool b2 -> Bool (b1 && b2)
      | _ -> failwith "Type error: And expects booleans"
    )
  | Or (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Bool b1, Bool b2 -> Bool (b1 || b2)
      | _ -> failwith "Type error: Or expects booleans"
    )
  | Add (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> failwith "Type error: Add expects integers"
    )
  | Sub (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> failwith "Type error: Sub expects integers"
    )
  | Mul (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Int (n1 * n2)
      | _ -> failwith "Type error: Mul expects integers"
    )
  | Eq (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Bool (n1 = n2)
      | _ -> failwith "Type error: Eq expects integers"
    )
  | Leq (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Bool (n1 <= n2)
      | _ -> failwith "Type error: Leq expects integers"
    )
;;

let eval_decl _ _ : state = failwith "TODO"

let trace _ _ = failwith "TODO"


