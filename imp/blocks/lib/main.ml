open Types
open Ast

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;


let apply st x =
  match topenv st x with
  | IVar l -> getmem st l
  | BVar l -> getmem st l

let rec eval_expr (st : state) (e : expr) : memval =
  match e with
  | True -> Bool true
  | False -> Bool false
  | Var x -> apply st x
  | Const n -> Int n
  | Not e1 -> 
    (
      match eval_expr st e1 with
      | Bool b -> Bool (not b)
      | _ -> failwith "Type error: Not expects a boolean"
    )
  | And (e1, e2) -> 
    (
      match (eval_expr st e1, eval_expr st e2) with
      | Bool b1, Bool b2 -> Bool (b1 && b2)
      | _ -> failwith "Type error: And expects booleans"
    )
  | Or (e1, e2) -> 
    (
      match (eval_expr st e1, eval_expr st e2) with
      | Bool b1, Bool b2 -> Bool (b1 || b2)
      | _ -> failwith "Type error: Or expects booleans"
    )
  | Add (e1, e2) -> 
    (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> failwith "Type error: Add expects integers"
    )
  | Sub (e1, e2) -> 
    (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> failwith "Type error: Sub expects integers"
    )
  | Mul (e1, e2) -> 
    (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Int (n1 * n2)
      | _ -> failwith "Type error: Mul expects integers"
    )
  | Eq (e1, e2) -> 
    (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Bool (n1 = n2)
      | _ -> failwith "Type error: Eq expects integers"
    )
  | Leq (e1, e2) -> 
    (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Bool (n1 <= n2)
      | _ -> failwith "Type error: Leq expects integers"
    )
;;

let rec eval_decl (st : state) (dls : decl list) : state =
  match dls with
  | [] -> st
  | IntVar(x)::dls' ->
      let loc = getloc st in
      let env = bind_env (topenv st) x (IVar loc) in
      let st' = setenv (setloc st (loc + 1)) (env :: getenv st) in
      eval_decl st' dls'
  | BoolVar(x)::dls' ->
      let loc = getloc st in
      let env = bind_env (topenv st) x (BVar loc) in
      let st' = setenv (setloc st (loc + 1)) (env :: getenv st) in
      eval_decl st' dls'
  ;;

let rec trace1 (c : conf) : conf =
  match c with
  | St _ -> raise NoRuleApplies
  | Cmd (Skip, st) -> St st
  | Cmd (Assign (x, e), st) -> 
  (
    match eval_expr st e with
    | Bool v -> 
    (
      match topenv st x with 
      | BVar l -> St (setmem st(bind_mem (getmem st) l (Bool v)))
      | _ -> failwith "Cannot assign Bool to Int var"
    )
    | Int v -> 
    (
      match topenv st x with 
      | IVar l -> St (setmem st(bind_mem (getmem st) l (Int v)))
      | _ -> failwith "Cannot assign Int to Bool var"
    )
  )
  | Cmd (Seq (c1, c2), st) -> 
    (
      match trace1 (Cmd (c1, st)) with
      | St st' -> Cmd (c2, st')
      | Cmd (c1', st') -> Cmd (Seq (c1', c2), st')
    )
  | Cmd (If (e, c1, c2), st) -> 
    (
      match eval_expr st e with
      | Bool true -> Cmd (c1, st)
      | Bool false -> Cmd (c2, st)
      | _ -> failwith "Type error: If condition must be boolean"
    )
  | Cmd (While (e, c), st) -> 
    (
      match eval_expr st e with
      | Bool true -> Cmd (Seq (c, While (e, c)), st)
      | Bool false -> St st
      | _ -> failwith "Type error: While condition must be boolean"
    )
  | Cmd (Decl ([], c), st) -> Cmd (c, st)
  | Cmd (Decl (dls, c), st) -> Cmd (Block c, eval_decl st dls)
  | Cmd (Block c, st) -> 
  (
    match trace1 (Cmd(c, st)) with
    | St st' -> St(setenv st'(popenv st))
    | Cmd(c',st') -> Cmd(Block c', st')
  )
  ;;

let trace (n : int) (c : cmd) : conf list = 
  let conf0 = Cmd (c, state0) in
  let rec helper i conf =
    if i >= n then [ conf ]
    else
      try conf :: helper (i + 1) (trace1 conf)
      with NoRuleApplies -> [ conf ]
  in
  helper 0 conf0
;;
  
