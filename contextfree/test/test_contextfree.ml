open Contextfree
open Types
open Grammar
open Exercises

let to_string = string_of_sentform

let%test "example_test" = derive todo [ 0; 1; 0; 1; 2 ] |> to_string = "01011010"

(** Following the example test, complete the tests below.

    For each test, provide the right sequence of productions
    to generate the word on the right of the "=".
*)

(* #### Exercise 1 *)
let%test "zero_n_one_n_1" = derive zero_n_one_n [ 1 ] |> to_string = ""
let%test "zero_n_one_n_2" = derive zero_n_one_n [ 0; 1 ] |> to_string = "01"
let%test "zero_n_one_n_3" = derive zero_n_one_n [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1 ] |> to_string = "00000000001111111111"

(* #### Exercise 2 *)
let%test "palindromes_1" = derive palindromes [ 1; 3; 4 ] |> to_string = "11011"
let%test "palindromes_2" = derive palindromes [ 0; 3; 6 ] |> to_string = "0110"
let%test "palindromes_3" = derive palindromes [ 0; 3; 3; 3; 4 ] |> to_string = "011101110"

let%test "palindromes_4" = derive palindromes2 [ 1; 1; 2; 4 ] |> to_string = "11011"
let%test "palindromes_5" = derive palindromes2 [ 0; 1; 4 ] |> to_string = "0110"
let%test "palindromes_6" = derive palindromes2 [ 0; 1; 1; 1; 2; 4 ] |> to_string = "011101110"

(* #### Exercise 3 *)
let%test "balanced_parentheses_1" = derive balanced_parentheses [ 6; 7; 8; 9 ] |> to_string = "()[]{}"
let%test "balanced_parentheses_2" = derive balanced_parentheses [ 4; 0; 2; 9 ] |> to_string = "({})[]"
let%test "balanced_parentheses_3" = derive balanced_parentheses [ 0; 2; 7; 1; 8; 0; 9 ] |> to_string = "({[][{}()]})"

(* #### Exercise 5 *)
let%test "zero_one_same_1" = derive same_amount [ 6 ] |> to_string = ""
let%test "zero_one_same_2" = derive same_amount [ 5; 4; 6 ] |> to_string = "1001"
let%test "zero_one_same_3" = derive same_amount [ 0; 4; 4; 6 ] |> to_string = "00110101"
let%test "zero_one_same_4" = derive same_amount [ 5; 0; 5; 6 ] |> to_string = "10001110"
