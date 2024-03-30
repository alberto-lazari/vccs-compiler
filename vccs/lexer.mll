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

(* Unicode alternatives *)
let tau      = "τ" | "tau"
let neq      = "≠" | "!=" | "/="
let leq      = "≤" | "<="
let geq      = "≥" | ">="

rule read = parse
  | blank+  { read lexbuf }

  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '['     { LBRACK }
  | ']'     { RBRACK }
  | '{'     { LBRACE }
  | '}'     { RBRACE }

  | '='     { EQ }
  | neq     { NEQ }
  | '<'     { LT }
  | '>'     { GT }
  | leq     { LEQ }
  | geq     { GEQ }

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
