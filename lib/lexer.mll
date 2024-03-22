{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '0'-'9' '_']+'\''?

rule read =
  parse
  | white { read lexbuf }

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

  | 'O' { NIL }
  | "τ" { TAU }
  | '\'' { TICK }
  | '.' { POINT }
  | "if" { IF }
  | "then" { THEN }
  | '|' { PIPE }
  | '\\' { BACKSLASH }

  | ',' { COMMA }
  | ';' { SEMICOLON }

  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  (* Has to be last, otherwise matches other keywords too *)
  | id { ID (Lexing.lexeme lexbuf) }

  | eof { EOF }
