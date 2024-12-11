%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token NOT
%token PLUS
%token MINUS
%token TIMES
%token EQUALS
%token LEQ
%token ASSIGN
%token WHILE
%token DO
%token SKIP
%token SEQ
%token <string> CONST
%token <string> VAR
%token EOF

%left OR
%left AND
%left NOT
%left EQ LEQ
%left ADD SUB
%left TIMES
%left SEQ
%nonassoc ELSE
%left DO

%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | NOT; e = expr { Not(e) } 
  | e1 = expr; AND; e2 = expr { And(e1,e2) }
  | e1 = expr; OR; e2 = expr { Or(e1,e2) }
  | LPAREN; e=expr; RPAREN { e }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | e1 = expr; TIMES; e2 = expr { Mul(e1,e2) }
  | e1 = expr; EQUALS; e2 = expr { Eq(e1,e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1,e2) }
  | n = CONST { Const(int_of_string n) }
  | v = VAR { Var(v) }
;

cmd:
  | SKIP { Skip }
  | v = VAR; ASSIGN; e = expr { Assign(v,e) }
  | c1 = cmd; SEQ; c2 = cmd { Seq(c1,c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e,c1,c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; LPAREN; c2 = cmd; RPAREN { If(e,c1,c2) }
  | IF; e=expr; THEN; LPAREN; c1=cmd; RPAREN; ELSE; c2=cmd { If(e,c1,c2) }
  | WHILE; e=expr; DO; c = cmd { While(e,c)}
  | WHILE; e=expr; DO; LPAREN; c = cmd; RPAREN { While(e,c)}
;
