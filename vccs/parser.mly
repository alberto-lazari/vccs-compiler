%{
  open Ast
%}

(* Tokens *)
%token <int> NUM
%token <string> ID

%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE

%token EQ NEQ
%token LT GT LEQ GEQ

%token PLUS MINUS TIMES SLASH
%token MOD

%token TRUE FALSE
%token NOT AND OR

%token ZERO
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
%left AND OR

%left PLUS MINUS
%left PIPE
%left POINT
%left BACKSLASH

%nonassoc THEN

%left TIMES SLASH
%left MOD


%start <Ast.prog> prog
%%

(* Syntax *)
abop_sum:
  | PLUS { Add }
  | MINUS { Sub }
abop_prod:
  | TIMES { Mult }
  | SLASH { Div }

expr:
  | LPAREN e = expr RPAREN { e }
  (* ZERO catches the representation of 0 before NUM *)
  | ZERO { Num 0 }
  | n = NUM { Num n }
  | x = ID { Var x }
  | e1 = expr; op = abop_sum; e2 = expr %prec PLUS {
    AritBinop (op, e1, e2)
  }
  | e1 = expr; op = abop_prod; e2 = expr %prec TIMES {
    AritBinop (op, e1, e2)
  }
  | e1 = expr MOD e2 = expr %prec MOD {
    AritBinop (Mod, e1, e2)
  }

bbop:
  | EQ { Eq }
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
  | e1 = expr; op = bbop; e2 = expr {
    BoolBinop (op, e1, e2)
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
  | ZERO { Nil }
  | a = act POINT p = proc { Act (a, p) }
  | k = ID { Const (k, []) }
  | k = ID LPAREN RPAREN { Const (k, []) }
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
  | k = ID EQ p = proc SEMICOLON pi = prog EOF {
    Def (k, [], p, pi)
  }
  | k = ID LPAREN RPAREN EQ p = proc SEMICOLON pi = prog EOF {
    Def (k, [], p, pi)
  }
  (* This doesn't work with params approach and needs a specific case, don't know why *)
  | k = ID LPAREN x = ID RPAREN EQ p = proc SEMICOLON pi = prog EOF {
    Def (k, [x], p, pi)
  }
  | k = ID LPAREN vars = params RPAREN EQ p = proc SEMICOLON pi = prog EOF {
    Def (k, vars, p, pi)
  }
