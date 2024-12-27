open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* Conta quante volte un elemento appare nella lista *)
let count x l =
  List.fold_left (fun acc y -> if x = y then acc + 1 else acc) 0 l

let freq lst =
  List.fold_left
    (fun acc x ->
       if List.mem_assoc x acc then
         List.map (fun (y, count) -> if y = x then (y, count + 1) else (y, count)) acc
       else
         (x, 1) :: acc)
    [] lst

let rec cut n lst =
  match (n, lst) with
  | (0, _) -> []
  | (_, []) -> []
  | (n, x::l) -> x :: (cut (n-1) l)

let frequency n lst = cut n (List.sort (fun (_,a) (_,b) -> b - a) (freq lst))
