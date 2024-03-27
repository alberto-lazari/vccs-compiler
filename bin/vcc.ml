open Vccs
open Encoder

let usage_msg = "Usage: vcc [-i min..max] <input-file> [-o <output-file>]\nOptions:"
let interval_string = ref "0..15"
let input_file = ref ""
let output_file = ref ""

let anon_fun file =
  input_file := file

let rec speclist =[
  (* Pass '-' as file to read stdin *)
  ("-", Arg.Unit (fun () -> input_file := "/dev/stdin"), "");
  ("-i", Arg.Set_string interval_string, "\t  values interval (ℕ set, default = {0..15})");
  ("-o", Arg.Set_string output_file, "\t  output file (default = <input-file-name>.ccs)");
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
  | Failure err -> Printf.eprintf "[%s]\n%s%!" file err


let _print_parsed_file (file : string) =
  print_iterated_file Main.parse_file Pretty_print.pp_prog file

let _print_encoded_file (interval : int * int) (file : string) =
  let module Interval = struct let interval = interval end in
  let open Encoder (Interval) in
  try print_iterated_file encode_file Ccs.Pretty_print.pp_prog file
  with Eval_error msg -> Printf.eprintf "[%s]\n%s%!" file msg

let compile (interval : int * int) (output_file : string) (input_file : string) =
  let module Interval = struct let interval = interval end in
  let open Encoder (Interval) in
  let out = open_out output_file in
  let fmt = Format.formatter_of_out_channel out in
  try Format.fprintf fmt "%a@."
    Ccs.Pretty_print.pp_prog (encode_file input_file);
    close_out out
  with
  | Eval_error msg ->
      Printf.eprintf "%s" msg;
      close_out out;
      exit (1)
  | Failure msg ->
      Printf.eprintf "%s" msg;
      close_out out;
      exit (1)


let () =
  Arg.parse speclist anon_fun usage_msg;
  let input_file = match !input_file with
  | "" ->
      Printf.eprintf "Error: no input files\n%s" (Arg.usage_string speclist usage_msg);
      exit (1)
  | file -> try match Sys.is_directory file with
      | true ->
          Printf.eprintf "Error: '%s' is a directory\n" file;
          exit (1)
      | false -> file
    with Sys_error _ ->
      Printf.eprintf "Error: file '%s' does not exist\n" file;
      exit (1)
  in

  let output_file = match !output_file with
  | "" -> if input_file = "/dev/stdin" then "/dev/stdout" else
    [ (* Change extension to .ccs *)
      ( (Str.regexp {|\([^.]*\)\.?[^.]*$|}), {|\1.ccs|} );
      (* Keep basename *)
      ( (Str.regexp {|\([^/]*/\)*\([^/]*\)|}), {|\2|} );
    ] |> List.fold_left
      (fun acc (pattern, replace) ->
        Str.replace_first pattern replace acc)
      input_file
  | "-" -> "/dev/stdout"
  | file -> file
  in

  let interval = Scanf.sscanf !interval_string
    "%d..%d"
    (fun min max -> (min, max))
  in
  compile interval output_file input_file
