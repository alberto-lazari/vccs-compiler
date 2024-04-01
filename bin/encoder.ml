module Encoder (Interval : sig
  val interval : int * int
end) = struct
  open Ccs.Ast
  module V = Vccs.Ast
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
    let v = Vccs.Eval.eval_expr e in
    if v < min || v > max then
      let msg = Printf.sprintf
        "Evaluation error: out of bounds value evaluated: %d\n"
        v
      in
      Eval_error msg |> raise
    else v

  let encode_act a = match a with
  | V.Tau -> Tau
  (* Discard var, assuming it has been already expanded *)
  | V.Input (ch_n, _) -> Input ch_n
  | V.Output (ch, e) -> Output (ch_n ch (eval_expr e))

  let rec encode_expanded_input p input =
    let encode_expanded_act p a =
      Act (encode_act a, encode_proc p)
    in
    match input with
    | V.Act (a, p) -> encode_expanded_act p a
    | V.Sum (V.Act (a, nextP), acts) ->
        Sum (encode_expanded_act nextP a, encode_expanded_input p acts)
    (* Dummy match, an expanded input should never be anything else *)
    | p -> encode_proc p

  and encode_proc p = match p with
    | V.Nil -> Nil
    | V.Act (a, p) -> begin match a with
        | V.Input (ch, x) ->
            expand_input domain p (ch, x) |> encode_expanded_input p
        | a -> Act (encode_act a, encode_proc p)
        end
    | V.Const (k, []) -> Const k
    | V.Const (k, args) ->
        let el = List.map (fun e -> eval_expr e |> string_of_int) args in
        let k_el = k ^ "_" ^ String.concat "," el in
        Const k_el
    | V.If (b, p) -> if Vccs.Eval.eval_boolean b
        then encode_proc p
        else Nil
    | V.Sum (p1, p2) -> Sum (encode_proc p1, encode_proc p2)
    | V.Paral (p1, p2) -> Paral (encode_proc p1, encode_proc p2)
    | V.Red (p, fs) ->
       Red (encode_proc p, expand_f domain fs)
    | V.Res (p, resL) -> Res (encode_proc p, expand_resL domain resL)

  let rec encode_prog ?(first=false) pi = match pi with
    | V.Proc p -> Proc (encode_proc p)
    | V.Def (k, [], p, pi) -> Def (k, encode_proc p, encode_prog pi)
    | pi -> expand_prog domain first pi |> encode_prog

  let encode_file file =
    let pi = Vccs.Main.parse_file file in
    try encode_prog ~first:true pi
    with Failure msg ->
      let msg = Printf.sprintf "Evaluation error: %s\n" msg in
      Eval_error msg |> raise
end
