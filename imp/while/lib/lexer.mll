{
open Parser
}

let white = [' ' '\t']+
let const = ['0' - '9']+
let var = ['a'-'z''A'-'Z']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "=" { EQUALS }
  | "<=" { LEQ }
  | ":=" { ASSIGN }
  | "while" { WHILE }
  | "do" { DO }
  | "skip" { SKIP }
  | ";" { SEQ }
  | const { CONST(Lexing.lexeme lexbuf) }
  | var { VAR(Lexing.lexeme lexbuf) }
  | eof { EOF }
