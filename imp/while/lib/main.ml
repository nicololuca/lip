open Ast
open Types

let bottom _ = failwith "Bottom doesn't contain any bindings";;

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let bind st x v : state = fun y -> if x=y then v else st y;;

let rec eval_expr : state -> expr -> exprval =
  fun st e ->
   match e with
   | True -> Bool true
   | False -> Bool false
   | And (e0, e1) -> (
       match (eval_expr st e0, eval_expr st e1) with
       | Bool b1, Bool b2 -> Bool (b1 && b2)
       | _ -> failwith "And accetta solo valori booleani")
   | Or (e0, e1) -> (
       match (eval_expr st e0, eval_expr st e1) with
       | Bool b1, Bool b2 -> Bool (b1 || b2)
       | _ -> failwith "Or accetta solo valori booleani")
   | Not e0 -> (
       match eval_expr st e0 with
       | Bool b1 -> Bool (not b1)
       | _ -> failwith "Not accetta solo valori booleani")
   | Add (e1, e2) -> (
       match (eval_expr st e1, eval_expr st e2) with
       | Nat n1, Nat n2 -> Nat (n1 + n2)
       | _ -> failwith "Add accetta solo Nat")
   | Sub (e1, e2) -> (
       match (eval_expr st e1, eval_expr st e2) with
       | Nat n1, Nat n2 -> Nat (n1 - n2)
       | _ -> failwith "Sub accetta solo Nat")
   | Mul (e1, e2) -> (
       match (eval_expr st e1, eval_expr st e2) with
       | Nat n1, Nat n2 -> Nat (n1 * n2)
       | _ -> failwith "Mul accetta solo Nat")
   | Eq (e1, e2) -> (
       match (eval_expr st e1, eval_expr st e2) with
       | Nat n1, Nat n2 -> Bool (n1 = n2)
       | Bool b1, Bool b2 -> Bool (b1 = b2)
       | _ -> failwith "Eq accetta solo valori con lo stesso tipo")
   | Leq (e1, e2) -> (
       match (eval_expr st e1, eval_expr st e2) with
       | Nat n1, Nat n2 -> Bool (n1 <= n2)
       | _ -> failwith "Leq accetta solo Nat")
   | Const n -> Nat n
   | Var v -> st v
  ;; 

let rec trace1 : conf -> conf = function
  (* Skip *)
  Cmd (Skip, state) -> St state
  (* Assign *)
| Cmd (Assign(x,e), state) -> 
    let new_state = bind state x (eval_expr state e) in St new_state
  (* Seq_St and Seq_Cmd *)
| Cmd (Seq(c1,c2), state) -> (
    match trace1 (Cmd (c1, state)) with
      Cmd (c1', state') -> Cmd (Seq (c1', c2), state')
    | St state' -> Cmd (c2, state')
  )
  (* If_True and If_False*)
| Cmd (If(e,c1,c2), state) -> (
    match (eval_expr state e) with
      Bool b -> if b then Cmd (c1, state) else Cmd (c2, state)
    | _ -> failwith "Il parametro per la condizione di If deve essere booleano"
  )
  (* While_True and While_False *)
| Cmd (While(e, c), state) -> (
    match (eval_expr state e) with
      Bool true -> Cmd (Seq (c, While (e, c)), state)
    | Bool false -> St state
    | _ -> failwith "Il parametro per la condizione di While deve essere booleano"
  )
| _ -> raise NoRuleApplies
;;

let trace (n_steps : int) (c : cmd) : conf list = 
  let c0 = Cmd (c, bottom) in
  let rec aux n conf =
    if n>0 then
      try
        let conf' = trace1 conf in
        conf :: aux (n-1) conf'
      with NoRuleApplies -> [ conf ]
    else [ conf ]
  in aux n_steps c0
;;
