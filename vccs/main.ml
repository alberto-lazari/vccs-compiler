let parse lexbuf =
  try
    Parser.prog Lexer.read lexbuf
  with
  | Parsing.Parse_error | Parser.Error | _ ->
    Error.report lexbuf |> failwith

let parse_file file =
  let c = open_in file in
  let lexbuf = Lexing.from_channel c in
  let pi = parse lexbuf in
  close_in c;
  pi
