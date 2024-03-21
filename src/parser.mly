%{
open Ast
%}

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

%token TAU
%token TICK
%token POINT
%token IF THEN
%token PIPE
%token BACKSLASH

%token COMMA
%token SEMICOLON

%token EOF



%nonassoc PIPE
%right POINT

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
%left MINUS
%left TIMES
%left SLASH
%left MOD


%start <Ast.prog> prog
%%

prog:
  | p = proc EOF { Proc p }
  | k = ID EQUALS p = proc SEMICOLON pi = prog EOF {
    Def (k, [], p, pi)
  }
  | k = ID LPAREN par = params RPAREN EQUALS p = proc SEMICOLON pi = prog EOF {
    Def (k, par, p, pi)
  }
