open Types
open Ast

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let bind st x v : state = fun y -> if x = y then v else st y
;;

let bottom : state = fun _ -> raise (UnboundVar "VAriable not found")
;;

let rec eval_expr (st : state) (e : expr) : exprval =
  match e with
  | True -> Bool true
  | False -> Bool false
  | Var x -> st x
  | Const n -> Nat n
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
      | Nat n1, Nat n2 -> Nat (n1 + n2)
      | _ -> failwith "Type error: Add expects integers"
    )
  | Sub (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Nat n1, Nat n2 -> Nat (n1 - n2)
      | _ -> failwith "Type error: Sub expects integers"
    )
  | Mul (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Nat n1, Nat n2 -> Nat (n1 * n2)
      | _ -> failwith "Type error: Mul expects integers"
    )
  | Eq (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Nat n1, Nat n2 -> Bool (n1 = n2)
      | _ -> failwith "Type error: Eq expects integers"
    )
  | Leq (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Nat n1, Nat n2 -> Bool (n1 <= n2)
      | _ -> failwith "Type error: Leq expects integers"
    )
;;

let rec trace1 (c : conf) : conf =
  match c with
  | St _ -> raise NoRuleApplies
  | Cmd (Skip, st) -> St st
  | Cmd (Assign (x, e), st) ->
      let v = eval_expr st e in
      St (bind st x v)
  | Cmd (Seq (c1, c2), st) -> (
      match trace1 (Cmd (c1, st)) with
      | St st' -> Cmd (c2, st')
      | Cmd (c1', st') -> Cmd (Seq (c1', c2), st')
    )
  | Cmd (If (e, c1, c2), st) -> (
      match eval_expr st e with
      | Bool true -> Cmd (c1, st)
      | Bool false -> Cmd (c2, st)
      | _ -> failwith "Type error: If condition must be boolean"
    )
  | Cmd (While (e, c), st) -> (
      match eval_expr st e with
      | Bool true -> Cmd (Seq (c, While (e, c)), st)
      | Bool false -> St st
      | _ -> failwith "Type error: While condition must be boolean"
    )
  ;;

let rec trace_step (n : int) (c : cmd) (st : state) : conf list =
  if n <= 0 then []
  else
    let conf = Cmd (c, st) in
    try
      let next_conf = trace1 conf in
      match next_conf with
      | St _ -> conf::[next_conf]
      | Cmd(c', st') -> conf :: trace_step (n-1) c' st'
    with NoRuleApplies -> [conf]
  ;;

let trace (n : int) (c : cmd) : conf list = trace_step n c bottom
