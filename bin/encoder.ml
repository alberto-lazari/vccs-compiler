module Encoder (Interval : sig val interval : int * int end) = struct
  open Ccs.Ast
  module Eval = Vccs.Eval
  module V = Vccs.Ast

  exception Eval_error of string

  let (min, max) = Interval.interval
  let eval_expr e =
    let v = Eval.eval_expr e in
    if v < min || v > max then
      let msg = Printf.sprintf
        "[!!] Evaluation error: out of bounds value evaluated: %d\n"
        v
      in
      Eval_error msg |> raise
    else
      v

  let encode_act a = match a with
    | V.Tau -> Tau
    (* TODO: Change with the actual behavior *)
    | V.Input (ch, _) -> Input ch
    | V.Output (ch, e) -> Output (ch ^ Printf.sprintf "_%d" (eval_expr e))

  let rec encode_proc p = match p with
    | V.Nil -> Nil
    | V.Act (a, p) -> Act (encode_act a, encode_proc p)
    | V.Const (k, args) ->
        let el = List.map (fun e -> eval_expr e |> string_of_int) args in
        begin match el with
        | [] -> Const k
        | _ ->
          let k_el = k ^ "_" ^ String.concat "," el in
          Const k_el
        end
    | V.If (b, p) -> if Eval.eval_boolean b
        then encode_proc p
        else Nil
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
    let pi = Vccs.Main.parse_file file in
    try encode_prog pi
    with Failure msg ->
      let msg = Printf.sprintf "[!!] Evaluation error: %s\n" msg in
      Eval_error msg |> raise
end
