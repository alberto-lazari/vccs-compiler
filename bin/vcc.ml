open Vcc_lib

let parse_file file =
  try Main.parse_file file
  with _ -> Format.printf "[%s]\n*error*\n" file

let rec parse_files files = match files with
  | [] -> failwith "no input files"
  | [file] -> parse_file file
  | file :: files ->
      parse_file file; print_endline "";
      parse_files files

let files = match Array.to_list Sys.argv with
  | [] -> []
  | _ :: fs -> fs

let _ = parse_files files
