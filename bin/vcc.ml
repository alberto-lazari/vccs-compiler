open Vccs

let print_parse_file file =
  try Format.printf "[%s]@.%a@.%!"
    file
    Pretty_print.pp_prog (Main.parse_file file)
  with
  | Sys_error err -> Printf.eprintf "[!!] %s\n" err
  | _ -> Format.printf "[%s]\n[!!] syntax error\n%!" file

let rec print_parse_files files = match files with
  | [] -> failwith "no input files"
  | [file] -> print_parse_file file
  | file :: files ->
      print_parse_file file; print_endline "";
      print_parse_files files

(* Discard $0 *)
let files = match Array.to_list Sys.argv with
  | [] -> []
  | _ :: fs -> fs

let () = print_parse_files files
