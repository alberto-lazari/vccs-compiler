let line lexbuf pos =
  let rec find_line line_num line_start =
    if line_start >= pos then
      line_num
    else begin
      let next_newline =
        try Some (Bytes.index_from lexbuf.Lexing.lex_buffer line_start '\n')
        with Not_found -> None in
      match next_newline with
      | Some next_newline ->
        find_line (line_num + 1) (next_newline + 1)
      | None ->
        line_num + 1
    end in
  let line = find_line 0 0 in
  line

let column lexbuf pos =
  let pos = pos - 1 in
  let rec newline_before_pos pos =
    if pos = 0 || Bytes.get lexbuf.Lexing.lex_buffer pos = '\n' then
      pos + 1
    else
      newline_before_pos (pos - 1) in

  let newline_pos = newline_before_pos pos - 1 in
  pos - newline_pos

let report lexbuf =
  let curr = Lexing.lexeme lexbuf in
  let pos = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum in
  let ln = line lexbuf pos in
  let col = column lexbuf pos in
  Format.sprintf "[!!] Syntax error at line %d col %d: unexpected token '%s'\n"
    ln col
    curr
