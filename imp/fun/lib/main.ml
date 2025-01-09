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
  | Call (_, _) ->
    (
      failwith "TODO"
    )
  | CallExec (_, _) ->
    (
      failwith "TODO"
    )
  | CallRet _ ->
    (
      failwith "TODO"
    ) 
;;

let rec eval_decl (st : state) (dls : decl list) : state =
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

and exec_cmd (st : state) (c : cmd) : state = 
    match c with
    | Skip -> st
    | Assign (x, e) -> 
      (
        let v = eval_expr st e in
        match topenv st x with 
        | IVar l -> setmem st (bind_mem (getmem st) l v)
        | IFun (_,_,_) -> failwith "Cannot assigned a function"
      )
    | Seq (c1, c2) -> 
      (
        let st' = exec_cmd st c1 in
        exec_cmd st' c2
      )
    | If (e, c1, c2) -> 
      (
        match eval_expr st e with
        | 0 -> exec_cmd st c2
        | _ -> exec_cmd st c1
      )
    | While (e, body) -> 
      (
        match eval_expr st e with
        | 0 -> st
        | _ -> exec_cmd (exec_cmd st body) c
      )

and trace1 (c : conf) : conf =
  match c with
  | St _ -> raise NoRuleApplies
  | Cmd (c, st) -> 
    let st' = exec_cmd st c in
    St st'
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

  
