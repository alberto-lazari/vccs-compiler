open Ccs.Ast

open Vccs.Eval
module V = Vccs.Ast

exception Eval_error of string

let encode_act a = match a with
  | V.Tau -> Tau
  (* TODO: Change with the actual behavior *)
  | V.Input (ch, _) -> Input ch
  | V.Output (ch, e) -> Output (ch ^ Printf.sprintf "_%d" (eval_expr e))

let rec encode_proc p = match p with
  | V.Nil -> Nil
  | V.Act (a, p) -> begin try
        Act (encode_act a, encode_proc p)
      with Failure msg ->
        let msg = Printf.sprintf "[!!] Evaluation error: %s\n" msg in
        Eval_error msg |> raise
  end
  (* TODO: Change with the actual behavior *)
  | V.Const (k, _) -> Const k
  | V.If (b, p) -> begin try
        if eval_boolean b then encode_proc p
        else Nil
      with Failure msg ->
        let msg = Printf.sprintf "[!!] Evaluation error: %s\n" msg in
        Eval_error msg |> raise
      end
  | V.Sum (p1, p2) -> Sum (encode_proc p1, encode_proc p2)
  | V.Paral (p1, p2) -> Paral (encode_proc p1, encode_proc p2)
  (* TODO: Change with the actual behavior *)
  | V.Red (p, fs) -> Red (encode_proc p, fs)
  (* TODO: Change with the actual behavior *)
  | V.Res (p, resL) -> Res (encode_proc p, resL)

let rec encode_prog pi = match pi with
  | V.Proc p -> Proc (encode_proc p)
  (* TODO: Change with the actual behavior *)
  | V.Def (k, _, p, pi) -> Def (k, encode_proc p, encode_prog pi)

let encode_file file =
  Vccs.Main.parse_file file |> encode_prog
