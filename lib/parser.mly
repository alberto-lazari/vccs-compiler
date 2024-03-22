%{
open Ast
%}

(* Tokens *)
%token <int> INT
%token <string> ID

%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE

%token EQUALS NEQ
%token LT GT
%token LEQ GEQ

%token PLUS MINUS
%token TIMES SLASH
%token MOD


%token TRUE FALSE
%token NOT AND OR

%token NIL
%token TAU
%token TICK
%token POINT
%token IF THEN
%token PIPE
%token BACKSLASH

%token COMMA
%token SEMICOLON

%token EOF


(* Association rules *)
%nonassoc THEN
%left EQUALS
%left NEQ
%left AND
%left OR
%left LT
%left GT
%left LEQ
%left GEQ

%left PLUS
%left PIPE
%right POINT


%start <Ast.prog> prog
%%

(* Syntax *)
abop:
  | PLUS { Add }
  | MINUS { Sub }
  | TIMES { Mult }
  | SLASH { Div }
  | MOD { Mod }

expr:
  | LPAREN e = expr RPAREN { e }
  | n = INT { Int n }
  (* NIL catches the representation of 0 before INT *)
  | NIL { Int 0 }
  | x = ID { Var x }
  | e1 = expr; op = abop; e2 = expr {
    AritBinop (op, e1, e2)
  }

bbop:
  | EQUALS { Eq }
  | NEQ { Neq }
  | LT { Lt }
  | GT { Gt }
  | LEQ { Leq }
  | GEQ { Geq }

boolean:
  | LPAREN b = boolean RPAREN { b }
  | TRUE { True }
  | FALSE { False }
  | NOT b = boolean { Not b }
  | b1 = boolean AND b2 = boolean { And (b1, b2) }
  | b1 = boolean OR b2 = boolean { Or (b1, b2) }
  | b1 = boolean; op = bbop; b2 = boolean {
    BoolBinop (op, b1, b2)
  }

act:
  | TAU { Tau }
  | a = ID LPAREN x = ID RPAREN {
    Input (a, x)
  }
  | TICK a = ID LPAREN e = expr RPAREN {
    Output (a, e)
  }

args:
  | e = expr { [e] }
  | e = expr COMMA exprs = args { e :: exprs }
params:
  | x = ID { [x] }
  | x = ID COMMA vars = params { x :: vars }
channels:
  | a = ID { [a] }
  | a = ID COMMA chs = channels { a :: chs }
redf:
  | a = ID SLASH b = ID { [(a, b)] }
  | a = ID SLASH b = ID COMMA reds = redf {
    (a, b) :: reds
  }
resL:
  | a = ID { [a] }
  | LBRACE chs = channels RBRACE { chs }

proc:
  | LPAREN p = proc RPAREN { p }
  | NIL { Nil }
  | a = act POINT p = proc { Act (a, p) }
  | k = ID { Const (k, []) }
  | k = ID LPAREN exprs = args RPAREN {
    Const (k, exprs)
  }
  | IF b = boolean THEN p = proc { If (b, p) }
  | p1 = proc PLUS p2 = proc { Sum (p1, p2) }
  | p1 = proc PIPE p2 = proc { Paral (p1, p2) }
  | p = proc LBRACK f = redf RBRACK { Red (p, f) }
  | p = proc BACKSLASH l = resL { Res (p, l) }

prog:
  | p = proc EOF { Proc p }
  | k = ID EQUALS p = proc SEMICOLON pi = prog EOF {
    Def (k, [], p, pi)
  }
  | k = ID LPAREN vars = params RPAREN EQUALS p = proc SEMICOLON pi = prog EOF {
    Def (k, vars, p, pi)
  }
