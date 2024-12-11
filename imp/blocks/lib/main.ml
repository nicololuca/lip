open Ast
open Types

let bottom _ = failwith "Bottom doesn't contain any bindings";;

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec eval_expr : state -> expr -> memval =
fun st e ->
  match e with
    True -> Bool true
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
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> failwith "Add accetta solo Int")
  | Sub (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> failwith "Sub accetta solo Int")
  | Mul (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Int (n1 * n2)
      | _ -> failwith "Mul accetta solo Int")
  | Eq (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Bool (n1 = n2)
      | Bool b1, Bool b2 -> Bool (b1 = b2)
      | _ -> failwith "Eq accetta solo valori con lo stesso tipo")
  | Leq (e1, e2) -> (
      match (eval_expr st e1, eval_expr st e2) with
      | Int n1, Int n2 -> Bool (n1 <= n2)
      | _ -> failwith "Leq accetta solo Int")
  | Const n -> Int n
  | Var v -> 
      let top_env = topenv st in (
        match top_env(v) with
          BVar l 
        | IVar l -> (getmem st)(l)
      )
;; 

let eval_decl : state -> decl list -> state =
fun st dl ->
  let (env, loc) = List.fold_left (
    fun (env, loc) d -> (
      match d with 
        IntVar x -> (bind_env env x (IVar loc), loc+1)
      | BoolVar x -> (bind_env env x (BVar loc), loc+1)
    )
  ) (topenv st, getloc st) dl in
  let envstack = getenv st in
  make_state (env::envstack) (getmem st) loc
;;

let rec trace1 : conf -> conf = function
  (* Skip *)
  Cmd (Skip, state) -> St state
  (* Assign *)
| Cmd (Assign(x, e), state) -> 
    let env = topenv state in
    let mem = getmem state in 
    let mem_val = (
      match eval_expr state e with
        Int n -> (
          match env(x) with
            IVar i -> bind_mem mem i (Int n)
          | _ -> failwith "Type mismatch"
        )
      | Bool b -> (
          match env(x) with
            BVar i -> bind_mem mem i (Bool b)
          | _ -> failwith "Type mismatch"
        )
    ) in St (make_state (getenv state) mem_val (getloc state))
  (* Seq_St and Seq_Cmd *)
| Cmd (Seq(c1, c2), state) -> (
    match trace1 (Cmd (c1, state)) with
      Cmd (c1', state') -> Cmd (Seq (c1', c2), state')
    | St state' -> (Cmd (c2, state'))
  )
  (* If_True and If_False*)
| Cmd (If(e, c1, c2), state) -> (
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
| Cmd (Decl (dl, c), state) -> 
    let new_state = eval_decl state dl in (
      match trace1 (Cmd (c, new_state)) with
        St st' -> St (make_state (popenv st') (getmem st') (getloc st'))
      | Cmd (c', state') -> Cmd (Block (c'), state')
    )
| Cmd (Block (c), state) -> (
    match trace1 (Cmd (c, state)) with
      St st' -> St (make_state (popenv st') (getmem st') (getloc st'))
    | Cmd (c', state') -> Cmd (Block (c'), state')
  )
| _ -> raise NoRuleApplies
;;

let trace (n_steps : int) (c : cmd) : conf list = 
  let c0 = Cmd (c, state0) in
  let rec aux n conf =
    if n>0 then
      try
        let conf' = trace1 conf in
        conf :: aux (n-1) conf'
      with NoRuleApplies -> [ conf ]
    else [ conf ]
  in aux n_steps c0
;;
