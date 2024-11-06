open Toylexer.Token
open Toylexer.Main

(*
let%test "test_frequencies_1" =
  lexer "x=y; x=x+1" |> frequency 3 = [(ID "x", 3); (ASSIGN, 2); (ID "y", 1)]

let%test "test_frequencies_2" =
  lexer "x=(x+2); y=x+1" |> frequency 1 = [(ID "x", 3)]

let%test "test_frequencies_3" =
  lexer "x;;; x+1" |> frequency 3 = [(SEQ, 3); (ID "x", 2); (PLUS, 1)]
*)

let%test "test_atok" =
  lexer "Ciao" = [ATOK "Ciao"; EOF]
let%test "test_btok" =
  lexer "aeiou" = [BTOK "aeiou"; EOF]
let%test "test_ctok" =
  lexer "test; test=x+1" |> frequency 2 = [(CTOK "test", 2); (SEQ, 1)]
let%test "test_dtok" =
  lexer "-3.14 -7. -.3" = [DTOK "-3.14"; DTOK "-7."; DTOK "-.3"; EOF]
let%test "test_etok" =
  lexer "0x44 0XACAA" = [ETOK "0x44"; ETOK "0XACAA"; EOF]