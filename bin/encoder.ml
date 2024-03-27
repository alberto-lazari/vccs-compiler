module Encoder (Interval : sig val interval : int * int end) = struct
  open Ccs.Ast
  module Eval = Vccs.Eval
  module V = Vccs.Ast
  open Vccs.Var_substitution
  open Vccs.Var_expansion

  exception Eval_error of string

  let (min, max) = Interval.interval
  let domain =
    let rec range min max =
      if min > max
      then []
      else min :: (range (min + 1) max)
    in
    range min max


  (* Catch out of bounds errors on final values *)
  let eval_expr e =
    let v = Eval.eval_expr e in
    if v < min || v > max then
      let msg = Printf.sprintf
        "Evaluation error: out of bounds value evaluated: %d\n"
        v
      in
      Eval_error msg |> raise
    else v

  let rec encode_act a nextP = match a with
    | V.Tau -> Act (Tau, encode_proc nextP)
    | V.Input (ch, x) ->
        let a n = Act (
          Input (ch_n ch n),
          encode_proc (substitute_proc_var x n nextP)
        ) in
        let rec input_expand domain = match domain with
          | [] -> Nil
          | n :: [] -> a n
          | n :: rest -> Sum (a n, input_expand rest)
        in
        input_expand domain
    | V.Output (ch, e) ->
        let a = Output (ch_n ch (eval_expr e)) in
        Act (a, encode_proc nextP)

  and encode_proc p = match p with
    | V.Nil -> Nil
    | V.Act (a, p) -> encode_act a p
    | V.Const (k, []) -> Const k
    | V.Const (k, args) ->
        let el = List.map (fun e -> eval_expr e |> string_of_int) args in
        let k_el = k ^ "_" ^ String.concat "," el in
        Const k_el
    | V.If (b, p) -> if Eval.eval_boolean b
        then encode_proc p
        else Nil
    | V.Sum (p1, p2) -> Sum (encode_proc p1, encode_proc p2)
    | V.Paral (p1, p2) -> Paral (encode_proc p1, encode_proc p2)
    | V.Red (p, fs) ->
       Red (encode_proc p, expand_f_var domain fs)
    | V.Res (p, resL) -> Res (encode_proc p, expand_resL_var domain resL)

  let rec encode_prog ?(first=false) pi = match pi with
    | V.Proc p -> Proc (encode_proc p)
    | V.Def (k, [], p, pi) -> Def (k, encode_proc p, encode_prog pi)
    | pi -> expand_prog_var domain first pi |> encode_prog

  let encode_file file =
    let pi = Vccs.Main.parse_file file in
    try encode_prog ~first:true pi
    with Failure msg ->
      let msg = Printf.sprintf "Evaluation error: %s\n" msg in
      Eval_error msg |> raise
end
