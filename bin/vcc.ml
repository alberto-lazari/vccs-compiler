open Vcc_lib

let _ =
  try
    Main.parse_file Sys.argv.(1)
  with
    Invalid_argument _ -> failwith "no input files"
