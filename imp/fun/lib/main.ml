open Types
open Ast

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let rec eval_expr (st : state) (e : expr) : memval =
  match e with
  | True -> 1
  | False -> 0
  | Var x -> apply st x
  | Const n -> n
  | Not e1 -> 
    (
      match eval_expr st e1 with
      | 0 -> 1
      | _ -> 0
    )
  | Or (e1, e2) 
  | Add (e1, e2) -> (eval_expr st e1) + (eval_expr st e2)
  | Sub (e1, e2) -> (eval_expr st e1) - (eval_expr st e2)
  | And (e1, e2)
  | Mul (e1, e2) -> (eval_expr st e1) * (eval_expr st e2)
  | Eq (e1, e2) -> 
    (
      match (eval_expr st e1, eval_expr st e2) with
      | n1, n2 when n1 = n2 -> 1
      | _ -> 0
    )
  | Leq (e1, e2) -> 
    (
      match (eval_expr st e1, eval_expr st e2) with
      | n1, n2 when n1 <= n2 -> 1
      | _ -> 0
    )
  | Call (name, arg) ->
    (
      let par, body, ret = apply_fun st name in
      let loc = getloc st in
      let arg_val = eval_expr st arg in
      let env = bind_env (topenv st) par (IVar loc) in
      let mem = bind_mem (getmem st) loc arg_val in
      let st' = setmem (setenv (setloc st (loc + 1)) (pushenv st env)) mem in
      eval_expr st' (CallExec(body, ret))
    )
  | CallExec (c, ret) ->
    (
      match trace1 (Cmd(c, st)) with
      | St st' -> eval_expr st' (CallRet(ret))
      | Cmd(c', st') -> eval_expr st' (CallExec(c', ret))
    )
  | CallRet ret -> eval_expr st ret
and eval_decl (st : state) (dls : decl list) : state =
  match dls with
  | [] -> st
  | IntVar(x)::dls' ->
    (
      let loc = getloc st in
      let env = bind_env (topenv st) x (IVar loc) in
      let st' = setenv (setloc st (loc + 1)) (env :: getenv st) in
      eval_decl st' dls'
    )
  | Fun (n, p, b, r) :: dls' ->
    (
      let env = bind_env (topenv st) n (IFun (p, b, r)) in
      eval_decl (setenv st (env :: getenv st)) dls'
    )
and trace1 (c : conf) : conf =
  match c with
  | St _ -> raise NoRuleApplies
  | Cmd (c, st) -> 
    (
      match c with 
      | Skip -> St st
      | Assign (x, e) -> St(bind_ivar st x (eval_expr st e))
      | Seq( c1, c2) ->
        (
          match trace1 (Cmd(c1, st)) with
          | St st' -> Cmd(c2, st')
          | Cmd (c', st') -> Cmd(Seq(c', c2), st')
        )
      | If (e, c1, c2) -> 
        (
          match eval_expr st e with
          | 0 -> Cmd(c1, st)
          | _ -> Cmd(c2, st)
        )
      | While (e, b) ->
        (
          match eval_expr st e with
          | 0 -> St st
          | _ -> Cmd(Seq(b, While(e, b)), st)
        )
    )
;;

let trace (n : int) (p : prog) : conf list = 
  let (dls, c) = match p with Prog(dls, c) -> (dls, c) in
  let st = eval_decl state0 dls in
  let conf0 = Cmd (c, st) in
  let rec helper i conf =
    if i >= n then [ conf ]
    else
      try conf :: helper (i + 1) (trace1 conf)
      with NoRuleApplies -> [ conf ]
  in
  helper 0 conf0
;;

  
