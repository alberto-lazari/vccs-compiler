open Vccs
open Encoder

let usage_msg = "usage: vcc [-i min..max] <input-file> [-o <output-file>]\noptions:"
let interval_string = ref "0..9"
let input_file = ref ""
let output_file = ref ""

let anon_fun file =
  input_file := file

let rec speclist =[
  ("-i", Arg.Set_string interval_string, "\t  values interval (ℕ set)");
  ("-o", Arg.Set_string output_file, "\t  output file name (default=input-file.ccs)");
  ("-help",
    Unit (fun _ -> Printf.eprintf "%s"
      (Arg.usage_string speclist usage_msg); exit 0),
    "  show this message");
  ("--help",
    Unit (fun _ -> Printf.eprintf "%s"
      (Arg.usage_string speclist usage_msg); exit 0),
    " show this message");
]

(* Prefix _ to suppress "unused variable" error *)
let rec _iterate_files (f : string -> unit) (files : string list) = match files with
  | [] | [""] ->
      Printf.eprintf "[!!] Error: no input files\n";
      exit (1)
  | [file] -> f file
  | file :: files ->
      f file; print_endline "";
      _iterate_files f files

let print_iterated_file (f : string -> 'a) (pp : Format.formatter -> 'a -> unit) (file : string) =
  try Format.printf "[%s]@.%a@.%!" file
    pp (f file)
  with
  | Sys_error err -> Printf.eprintf "[!!] System error%s\n%!" err
  | Failure e -> Printf.eprintf "[%s]\n%s%!" file e


let _print_parsed_file (file : string) =
  print_iterated_file Main.parse_file Pretty_print.pp_prog file

let _print_encoded_file (interval : int * int) (file : string) =
  let module Interval = struct let interval = interval end in
  let open Encoder (Interval) in
  try print_iterated_file encode_file Ccs.Pretty_print.pp_prog file
  with Eval_error msg -> Printf.eprintf "[%s]\n%s%!" file msg

let compile (interval : int * int) =
  let module Interval = struct let interval = interval end in
  let open Encoder (Interval) in
  try Format.printf "%a@."
    Ccs.Pretty_print.pp_prog (encode_file !input_file)
  with Eval_error msg ->
    Printf.eprintf "%s" msg;
    exit (1)


let () =
  Arg.parse speclist anon_fun usage_msg;
  let _output_file = if !output_file = ""
    then Str.replace_first
      (Str.regexp {|\(.*\)\.vccs|}) {|\1.ccs|}
      !input_file
    else !output_file
  in
  let interval = Scanf.sscanf !interval_string
    "%d..%d"
    (fun min max -> (min, max))
  in
  compile interval
