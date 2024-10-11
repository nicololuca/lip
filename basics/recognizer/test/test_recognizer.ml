open Recognizer

(* lang1 - [01]+ *)
let%test _ = lang1 [] = false
let%test _ = lang1 ['0';'1';'0';'1']
let%test _ = lang1 ['0';'0';'0';'0';'1']

(* lang2 - 0?1* *)
let%test _ = lang2 []
let%test _ = lang2 ['0']
let%test _ = lang2 ['0';'0'] = false
let%test _ = lang2 ['0';'1']
let%test _ = lang2 ['0';'1';'1']

(* lang3 - 0[01]*0 *)
let%test _ = lang3 [] = false
let%test _ = lang3 ['0'] = false
let%test _ = lang3 ['0';'0']
let%test _ = lang3 ['0';'0';'1'] = false
let%test _ = lang3 ['1';'0';'0'] = false
let%test _ = lang3 ['0';'0';'0']
let%test _ = lang3 ['0';'1';'0']
let%test _ = lang3 ['0';'1';'1';'0']

(* lang4 - 0*10*10* *)
let%test _ = lang4 [] = false
let%test _ = lang4 ['1';'1']
let%test _ = lang4 ['0';'1'] = false
let%test _ = lang4 ['0';'1';'1']
let%test _ = lang4 ['1';'0';'1']
let%test _ = lang4 ['1';'1';'1'] = false
let%test _ = lang4 ['0';'0';'1';'1';'0']
let%test _ = lang4 ['1';'0';'0';'1';'0']

(* lang5 - (00|11)+ *)
let%test _ = lang5 [] = false
let%test _ = lang5 ['0';'0';'1';'1']
let%test _ = lang5 ['0';'0';'1';'1';'0';'0']
let%test _ = lang5 ['0';'0';'1';'1';'0'] = false
let%test _ = lang5 ['0';'0']
let%test _ = lang5 ['1';'1']
let%test _ = lang5 ['0';'1'] = false
