let parse lexbuf =
  try
    Parser.prog Lexer.read lexbuf
  with
  | Parsing.Parse_error | Parser.Error | _ ->
    let curr = Lexing.lexeme lexbuf in
    let pos = Lexing.lexeme_start lexbuf in
    let spaces = String.make pos ' ' in
    Format.sprintf "%s^\n[!!] Syntax error at position %d: Unexpected token '%s'\n"
      spaces pos curr |> failwith

let parse_file file =
  let c = open_in file in
  let lexbuf = Lexing.from_channel c in
  let pi = parse lexbuf in
  close_in c;
  pi
