open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type int_or_err = (int, string) Result.t

let ( ==> ) (res : int_or_err) (f : int -> int_or_err) : int_or_err =
  match res with
  | Ok value -> f value
  | Error msg -> Error msg

let string_of_intorerr : int_or_err -> string = function
  | Ok n -> string_of_int n
  | Error msg -> msg

(* eval : ast -> result *)

let rec eval : ast -> int_or_err = function
  | Const n -> Ok n
  | Hex n -> Ok n
  | Add (e1,e2) ->
    (
      let res1 = eval e1 in
      let res2 = eval e2 in
      match res1, res2 with
      | Error err1, _ -> Error err1
      | _, Error err2 -> Error err2
      | Ok v1, Ok v2 -> Ok (v1 + v2)
    )
  | Sub (e1,e2) ->
    (
      let res1 = eval e1 in
      let res2 = eval e2 in
      match res1, res2 with
      | Error err1, _ -> Error err1
      | _, Error err2 -> Error err2
      | Ok v1, Ok v2 -> Ok (v1 - v2)
    )
  | Div (e1,e2) ->
    (
      let res1 = eval e1 in
      let res2 = eval e2 in
      match res1, res2 with
      | Error err1, _ -> Error err1
      | _, Error err2 -> Error err2
      | Ok v1, Ok 0 -> Error ("tried to divide " ^ string_of_int(v1) ^ " by zero")
      | Ok v1, Ok v2 -> Ok (v1 / v2)
    )
  | Neg e1 ->
    (
      let res1 = eval e1 in
      match res1 with
      | Error err1 -> Error err1
      | Ok v1 -> Ok (-v1)
    )