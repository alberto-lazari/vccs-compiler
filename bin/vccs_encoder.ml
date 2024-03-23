open Ccs.Ast
module V = Vccs.Ast

let encode_act a = match a with
  | V.Tau -> Tau
  (* TODO: Change with the actual behavior *)
  | V.Input (ch, _) -> Input ch
  (* TODO: Change with the actual behavior *)
  | V.Output (ch, _) -> Output ch

let rec encode_proc p = match p with
  | V.Nil -> Nil
  | V.Act (a, p) -> Act (encode_act a, encode_proc p)
  (* TODO: Change with the actual behavior *)
  | V.Const (k, _) -> Const k
  (* TODO: Change with the actual behavior *)
  | V.If (_, p) -> encode_proc p
  | V.Sum (p1, p2) -> Sum (encode_proc p1, encode_proc p2)
  | V.Paral (p1, p2) -> Paral (encode_proc p1, encode_proc p2)
  | V.Red (p, fs) -> Red (encode_proc p, fs)
  | V.Res (p, resL) -> Res (encode_proc p, resL)

let rec encode_prog pi = match pi with
  | V.Proc p -> Proc (encode_proc p)
  (* TODO: Change with the actual behavior *)
  | V.Def (k, _, p, pi) -> Def (k, encode_proc p, encode_prog pi)

let encode_file file =
  Vccs.Main.parse_file file |> encode_prog
