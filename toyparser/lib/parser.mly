%{
open Ast
%}

%token <string> CONST
%token <string> HEX_CONST
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token LPAREN
%token RPAREN
%token EOF

%left PLUS MINUS
%left TIMES DIVIDE

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | n = HEX_CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | e1 = expr; TIMES; e2 = expr { Mul(e1,e2) }
  | e1 = expr; DIVIDE; e2 = expr { Div(e1,e2) }
  | MINUS; e = expr { Sub(Const 0,e) }
  | LPAREN; e=expr; RPAREN {e}
;
