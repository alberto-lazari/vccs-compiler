{
open Parser
}

let blank = [' ' '\t' '\n']+
let digit = ['0'-'9']
let integer = '-'? digit+

let letter = ['a'-'z' 'A'-'Z']
let alphanumeric = letter | digit
let id = letter
  (* Allow special symbols in the middle *)
  (alphanumeric | ['_' '-'])*
  (* Do not end with special symbols *)
  alphanumeric?
  (* Allow primes *)
  '\''*

rule read = parse
  | blank+ { read lexbuf }

  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '{' { LBRACE }
  | '}' { RBRACE }

  | '=' { EQUALS }
  | "≠" { NEQ }
  | '<' { LT }
  | '>' { GT }
  | "≤" { LEQ }
  | "≥" { GEQ }

  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { SLASH }
  | "mod" { MOD }

  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }

  | '0' { ZERO }
  | "τ" { TAU }
  | '\'' { TICK }
  | '.' { POINT }
  | "if" { IF }
  | "then" { THEN }
  | '|' { PIPE }
  | '\\' { BACKSLASH }

  | ',' { COMMA }
  | ';' { SEMICOLON }

  | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
  (* Has to be last, otherwise matches other keywords too *)
  | id { ID (Lexing.lexeme lexbuf) }

  | eof { EOF }
