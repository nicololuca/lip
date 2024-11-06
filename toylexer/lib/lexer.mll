{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

let vowel = ['a' 'A' 'e' 'E' 'i' 'I' 'o' 'O' 'u' 'U']
let lowercase_vowel = ['a' 'e' 'i' 'o' 'u']
let consonant = letter # vowel
let digit = ['0'-'9']
let hex = ['0'-'9' 'A'-'F' 'a'-'f']

let atok = ['A'-'Z'] chr*
let btok = lowercase_vowel+
let ctok = consonant* vowel? consonant*
let dtok = ['-']? digit* ['.']? digit*
let etok = '0' ('x'|'X') hex+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  (* a to e tokens have to be declared before id and num in order to give them priority *)
  | atok { ATOK (Lexing.lexeme lexbuf) }  
  | btok { BTOK (Lexing.lexeme lexbuf) }  
  | ctok { CTOK (Lexing.lexeme lexbuf) }  
  | dtok { DTOK (Lexing.lexeme lexbuf) }  
  | etok { ETOK (Lexing.lexeme lexbuf) }  
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
