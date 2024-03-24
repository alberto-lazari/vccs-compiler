open Vccs
open Encoder

module Interval = struct
  let interval = (0, 2)
end
open Encoder (Interval)

let rec iterate_files (f : string -> unit) (files : string list) = match files with
  | [] -> failwith "no input files"
  | [file] -> f file
  | file :: files ->
      f file; print_endline "";
      iterate_files f files

let print_iterated_file (f : string -> 'a) (pp : Format.formatter -> 'a -> unit) (file : string) =
  try Format.printf "[%s]@.%a@.%!" file
    pp (f file)
  with
  | Sys_error err -> Printf.eprintf "[!!] %s\n%!" err
  | Failure e -> Printf.eprintf "[%s]\n%s%!" file e


(* Prefix _ to suppress "unused variable" error *)
let _print_parsed_file (file : string) =
  print_iterated_file Main.parse_file Pretty_print.pp_prog file

let print_encoded_file (file : string) =
  try print_iterated_file encode_file Ccs.Pretty_print.pp_prog file
  with Eval_error msg -> Printf.eprintf "[%s]\n%s%!" file msg


(* Discard $0 *)
let files = match Array.to_list Sys.argv with
  | [] -> []
  | _ :: fs -> fs

let () = iterate_files print_encoded_file files
