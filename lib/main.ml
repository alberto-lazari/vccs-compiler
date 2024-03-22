let parse_file file =
  let c = open_in file in
  let lexbuf = Lexing.from_channel c in
  let pi = Parser.prog Lexer.read lexbuf in
  close_in c;
  Format.printf "[%s]@.%a@.%!"
    file
    Pretty_print.pp_prog pi
