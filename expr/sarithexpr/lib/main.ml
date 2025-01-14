open Ast
type exprval = Bool of bool | Nat of int
type exprtype = BoolT | NatT

let string_of_val = function
| Bool e -> string_of_bool e
| Nat e -> string_of_int e

let string_of_type = function
| NatT -> "Nat"
| BoolT -> "Bool"

let rec string_of_expr = function
  | True -> "true"
  | False -> "false"
  | Zero -> "0"
  | Succ(e) -> "succ(" ^ (string_of_expr e) ^ ")"
  | Pred(e) -> "pred(" ^ (string_of_expr e) ^ ")"
  | IsZero(e) -> "iszero(" ^ (string_of_expr e) ^ ")"
  | If(e0, e1, e2) -> 
      "if(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "not(" ^ (string_of_expr e) ^ ")"
  | And(e1, e2) -> "and(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Or(e1, e2) -> "or(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let rec is_nv = function
| Zero -> true
| Succ e' -> is_nv e'
| _ -> false


exception NoRuleApplies
  
let not x = if (x = true) then false else true ;;

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 e in Not(e')
  | And(True, e2) -> e2
  | And(False, _) -> False
  | And (e1, e2) -> let e1' = trace1 e1 in And(e1', e2)
  | Or(True, _) -> True
  | Or(False, e2) -> e2
  | Or(e1, e2) -> let e1' = trace1 e1 in Or(e1', e2)
  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Succ(nv)) when is_nv nv -> nv
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when is_nv e -> False
  | IsZero(e) -> let e' = trace1 e in IsZero(e')
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

let rec eval = function
    True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | If(e0,e1,e2) -> 
    (
      match eval e0 with
      | Bool true -> eval e1
      | Bool false -> eval e2
      | _ -> failwith "Error"
    )
  | Not(e) -> 
    (
      match eval e with
      | Bool e' -> Bool (not e')
      | _ -> failwith "Error"
    )
  | And(e0,e1) -> 
    (
      match eval e0, eval e1 with
      | (Bool e0', Bool e1') -> Bool(e0' && e1')
      | _ -> failwith "Error"
    )
  | Or(e0,e1) -> 
    (
      match eval e0, eval e1 with
      | (Bool e0', Bool e1') -> Bool(e0' || e1')
      | _ -> failwith "Error"
    )
  | IsZero(e) -> 
    (
      match eval e with
      | Nat e -> Bool (e == 0)
      | _ -> failwith "Error"
    )
  | Succ(e) ->
    (
      match eval e with
      | Nat e -> Nat(1 + e)
      | _ -> failwith "Error"
    )
  | Pred(e) -> 
    (
      match eval e with
      | Nat e when e != 0 -> Nat(e - 1)
      | _ -> failwith "Error"
    )

;;

exception TypeError of string

let raise_type_error expr expected found = 
  raise (TypeError ((string_of_expr expr) ^ " has type " ^ 
                    (string_of_type found) ^ ", but type " ^ 
                    (string_of_type expected) ^ " was expected"))

let rec typecheck e =
  let typecheck_and_raise expr expected =
    let found = typecheck expr in
    if found != expected then raise_type_error expr expected found;
    found
  in
  match e with
  | True -> BoolT
  | False -> BoolT
  | Zero -> NatT
  | Succ(e) -> typecheck_and_raise e NatT |> fun _ -> NatT
  | Pred(e) -> typecheck_and_raise e NatT |> fun _ -> NatT
  | IsZero(e) -> typecheck_and_raise e NatT |> fun _ -> BoolT
  | Not(e) -> typecheck_and_raise e BoolT |> fun _ -> BoolT
  | And(e1, e2) -> 
      typecheck_and_raise e1 BoolT |> fun _ ->
      typecheck_and_raise e2 BoolT |> fun _ -> BoolT
  | Or(e1, e2) -> 
      typecheck_and_raise e1 BoolT |> fun _ ->
      typecheck_and_raise e2 BoolT |> fun _ -> BoolT
  | If(e0, e1, e2) -> 
      typecheck_and_raise e0 BoolT |> fun _ ->
      let t1 = typecheck e1 in
      let t2 = typecheck e2 in
      if t1 != t2 then raise_type_error e2 t1 t2 else t1