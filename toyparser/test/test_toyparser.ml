open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9
let%test "test_eval_2" = parse "1 + 9" |> eval = Ok 10
(* YOUR TESTS HERE *)