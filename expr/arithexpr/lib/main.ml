open Ast

type exprval = Bool of bool | Nat of int

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Not e -> "Not(" ^ (string_of_expr e) ^ ")"
  | And(e0,e1) -> "And(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | Or(e0,e1) -> "Or(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ e -> "Succ(" ^ (string_of_expr e) ^ ")"
  | Pred e -> "Pred(" ^ (string_of_expr e) ^ ")"
  | IsZero e -> "IsZero(" ^ (string_of_expr e) ^ ")"

let string_of_val = function
    Bool true -> "true"
  | Bool false -> "false"
  | Nat v -> string_of_int v

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec is_nv = function
    Zero -> true
  | Succ e1 -> is_nv e1
  | _ -> false

exception NoRuleApplies

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> 
      If(trace1 e0,e1,e2)
  | Not True -> False
  | Not False -> True
  | Not e -> Not(trace1 e)
  | And(True,e2) -> e2
  | And(False,_) -> False
  | And(e1,e2) -> And(trace1 e1, e2)
  | Or(True,_) -> True
  | Or(False,e2) -> e2
  | Or(e1,e2) -> Or(trace1 e1,e2)
  | Pred(Succ(e)) -> e
  | Succ(e) -> Succ(trace1 e)
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> True
  | IsZero(Succ _) -> False
  | IsZero(e) -> IsZero(trace1 e)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

let bool_of_exprval v = match v with
    Bool true -> true
  | Bool false -> false
  | _ -> failwith "Not a bool"
;;
let value_of_exprval v = match v with
    Nat n -> n
  | _ -> failwith "Not a nat"
;;

let rec eval : expr -> exprval = function
    True -> Bool true
  | False -> Bool false
  | Not e -> Bool(not(bool_of_exprval (eval e)))
  | And(e0,e1) -> Bool((bool_of_exprval (eval e0)) && (bool_of_exprval (eval e1)))
  | Or(e0,e1) -> Bool((bool_of_exprval (eval e0)) || (bool_of_exprval (eval e1)))
  | If(e0,e1,e2) -> 
      if (bool_of_exprval (eval e0)) then (eval e1) else (eval e2)
  | Zero -> Nat(0)
  | Succ(e) -> Nat(value_of_exprval (eval e) + 1)
  | Pred(e) -> let n = value_of_exprval (eval e) in
      if n>0 then Nat (n-1) else failwith "Not a natural number"
  | IsZero(e) -> let n = value_of_exprval (eval e) in
      if n=0 then Bool true else Bool false
;;
