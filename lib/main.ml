let parse (file : string) =
  let c = open_in file in
  let lexbuf = Lexing.from_channel c in
  let p = Parser.prog Lexer.read lexbuf in
  close_in c;
  p
