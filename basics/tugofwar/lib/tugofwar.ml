(* tokens *)
type token = A | B | X

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
let gen_token c = match c with
  'A' -> A
| 'B' -> B
| '=' -> X
| _ -> failwith "Token non valido"
;;
let toklist_of_string s = List.map gen_token (explode s);;

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let rec valid l = match l with
  [A] | [B] | [X] -> true
| A::l1 | B::l1 | X::l1 -> valid l1
| _ -> false
;;

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

(* This method counts the tokens using an auxiliary function. *)
(* 
let rec win_counter l (a,b) = match l with
  [] -> (a,b)
| x::l1 -> match x with
    A -> win_counter l1 (a+1,b)
  | B -> win_counter l1 (a,b+1)
  | X -> win_counter l1 (a,b)
;;
let win l = 
  let (a,b) = win_counter l (0,0) in
  if a>b then A else if b>a then B else X
;; 
*)

(* This method does not directly count the occurrences of tokens, *)
(* but filters the list twice leaving only the desired tokens and *)
(* calculates its length. Whichever one has the longest list, wins. *)
(* If both lists are the same length, it's a tie. *)
let win l =
  let a = List.length (List.filter (fun x -> x=A) l) in
  let b = List.length (List.filter (fun x -> x=B) l) in
  if a>b then A else if b>a then B else X
;;

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with
  A -> "A"
| B -> "B"
| X -> "Tie"
;;
