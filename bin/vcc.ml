open Vcc_lib

let parse_file file =
  try Main.parse_file file
  with
  | Sys_error err -> Printf.eprintf "[!!] %s\n" err
  | _ -> Format.printf "[%s]\n[!!] syntax error\n%!" file

let rec parse_files files = match files with
  | [] -> failwith "no input files"
  | [file] -> parse_file file
  | file :: files ->
      parse_file file; print_endline "";
      parse_files files

(* Discard $0 *)
let files = match Array.to_list Sys.argv with
  | [] -> []
  | _ :: fs -> fs

let () = parse_files files
