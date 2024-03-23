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

let get_entire_line_at_position lexbuf pos =
  let buffer_length = Bytes.length lexbuf.Lexing.lex_buffer in
  if buffer_length = 0 then
    "" (* Return an empty string if the buffer is empty *)
  else
    let rec find_line_start end_pos =
      if end_pos = 0 || Bytes.get lexbuf.Lexing.lex_buffer (end_pos - 1) = '\n' then
        end_pos
      else
        find_line_start (end_pos - 1)
    in
    let rec find_line_end start_pos =
      if start_pos = buffer_length || Bytes.get lexbuf.Lexing.lex_buffer start_pos = '\n' then
        start_pos
      else
        find_line_end (start_pos + 1)
    in
    let line_start_pos = find_line_start pos in
    let line_end_pos = find_line_end pos in
    let line_content = Bytes.sub_string lexbuf.Lexing.lex_buffer line_start_pos (line_end_pos - line_start_pos) in
    line_content

let report lexbuf =
  let curr = Lexing.lexeme lexbuf in
  let pos = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum in
  let ln = line lexbuf pos in
  let col = column lexbuf pos in
  let line = get_entire_line_at_position lexbuf pos in
  let marker = String.make (col - 1) ' ' ^ "^" in
  Format.sprintf "%s\n%s\n[!!] Syntax error at line %d col %d: unexpected token '%s'\n"
    line
    marker
    ln col
    curr
