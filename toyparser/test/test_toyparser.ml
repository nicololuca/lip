open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

(* YOUR TESTS HERE *)

let%test "test_eval_2" = parse "(1 - 2) + 4" |> eval = Ok 3
let%test "test_eval_3" = parse "1 + 2 / 0" |> eval = Error "Error: tried to divide 2 by zero"
let%test "test_eval_3_bis" = parse "1 + 5 / 0" |> eval = Error "Error: tried to divide 5 by zero"
let%test "test_eval_4" = parse "-1 - 2 - -3" |> eval = Ok 0
let%test "test_eval_4_bis" = parse "-1-2--3" |> eval = Ok 0
let%test "test_eval_4_ter" = parse "-1" |> eval = Ok (-1)
let%test "test_eval_5" = parse "0x01 + 2" |> eval = Ok 3
let%test "test_eval_5_bis" = parse "0X01 + 5" |> eval = Ok 6
let%test "test_eval_6" = parse "6 * 2 + 3" |> eval = Ok 15
let%test "test_eval_7" = parse "6 * (2 + 3)" |> eval = Ok 30
let%test "test_eval_8" = parse "6 / 2 * 3" |> eval = Ok 9
let%test "test_eval_9" = parse "6 * 2 / 3" |> eval = Ok 4