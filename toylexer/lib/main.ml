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

(* Auxiliary functions for frequency *)
(* single_tok_frequency : token -> 'a list -> int filters the given token list in order to keep 
   only the desired token, and returns its occurrences by calculating the length of the filtered list *)
let single_tok_frequency t toklist = List.length ( List.filter (fun y -> y=t) toklist )
(* tok_pairs : 'a list -> ('a * int) list returns the list of each token with its occurrences, 
   calculated by recursively calling itself passing as a parameter a new list obtained by 
   removing every occurrence of the token that was just analyzed *)
let rec tok_pairs toklist = match toklist with
  [] -> []
| x::l1 -> (x, single_tok_frequency x toklist)::(tok_pairs (List.filter (fun y -> not(x=y)) l1))
;;

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n toklist = 
  let pairs = List.sort (fun (_, count1) (_, count2) -> compare count2 count1) (tok_pairs toklist) in
  let rec first_n_elements l n = match l with
    [] -> []
  | x::l1 when n>0 -> x::first_n_elements l1 (n-1)
  | _ -> []
  in first_n_elements pairs n
;;
