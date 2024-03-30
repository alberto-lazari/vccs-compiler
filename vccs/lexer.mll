{
  open Parser
}

let blank    = [' ' '\t' '\n']+
let digit    = ['0'-'9']
let number   = digit+

let letter   = ['a'-'z' 'A'-'Z']
let alphanum = letter | digit
let id       = letter
               (* Allow special symbols in the middle *)
               (alphanum | ['_' '-'])*
               (* Do not end with special symbols *)
               alphanum?
               (* Allow primes *)
               '\''*

let tau      = "τ" | "tau"

rule read = parse
  | blank+  { read lexbuf }

  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '['     { LBRACK }
  | ']'     { RBRACK }
  | '{'     { LBRACE }
  | '}'     { RBRACE }

  | '='     { EQ }
  | "≠"     { NEQ }
  | '<'     { LT }
  | '>'     { GT }
  | "≤"     { LEQ }
  | "≥"     { GEQ }

  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { SLASH }
  | "mod"   { MOD }

  | "true"  { TRUE }
  | "false" { FALSE }
  | "not"   { NOT }
  | "and"   { AND }
  | "or"    { OR }

  | '0'     { ZERO }
  | tau     { TAU }
  | '\''    { TICK }
  | '.'     { POINT }
  | "if"    { IF }
  | "then"  { THEN }
  | '|'     { PIPE }
  | '\\'    { BACKSLASH }

  | ','     { COMMA }
  | ';'     { SEMICOLON }

  | number  { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  (* Has to be last, otherwise matches other keywords too *)
  | id      { ID (Lexing.lexeme lexbuf) }

  | eof     { EOF }
