{
  open Parser
}

let white = [' ' '\t']+
let seq = ['0'-'9']+
let ext_seq = ['0'-'9']+(','['0'-'9']+)*
let range = ['0'-'9']+".."['0'-'9']+
let range_seq = range (','range)*

rule read_token =
  parse
  | white { read_token lexbuf } 
  | "/" { SLASH }
  | "E" { EXT }
  | "S" { S_TOK }
  | "B" { B_TOK }
  | seq { SEQ (Lexing.lexeme lexbuf) }
  | ext_seq { SEQ (Lexing.lexeme lexbuf) }
  | range_seq { RANGE_SEQ (Lexing.lexeme lexbuf) }
  | eof { EOF }
