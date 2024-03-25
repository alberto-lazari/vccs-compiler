module Encoder (Interval : sig val interval : int * int end) = struct
  open Ccs.Ast
  module Eval = Vccs.Eval
  module V = Vccs.Ast
  open Vccs.Substitution

  exception Eval_error of string

  let (min, max) = Interval.interval
  let domain =
    let rec range ?(start=0) len =
      if start > len
      then []
      else start :: (range len ~start:(start+1))
    in
    range ~start:min max


  (* Catch out of bounds errors on final values *)
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

  (* Convention: value 0 creates the pure CCS channel *)
  let ch_n ch n = if n != 0
    then Printf.sprintf "%s_%d" ch n
    else ch

  let rec encode_act a nextP = match a with
    | V.Tau -> Act (Tau, encode_proc nextP)
    | V.Input (ch, x) ->
        let rec input_expand domain = match domain with
          | [] -> Nil
          | n :: [] ->
              Act (Input (ch_n ch n), encode_proc (substitute_proc_var x n nextP))
          | n :: rest ->
              Sum (
                Act (Input (ch_n ch n), encode_proc (substitute_proc_var x n nextP)),
                input_expand rest
              )
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
        let rec fs_expand fs = match fs with
          | [] -> []
          | (a, b) :: rest -> List.map
                (fun n -> (ch_n a n, ch_n b n))
                domain
              @
              fs_expand rest
        in
       Red (encode_proc p, fs_expand fs)
    | V.Res (p, resL) ->
        let rec resL_expand l = match l with
          | [] -> []
          | a :: acts -> List.map (fun n -> ch_n a n) domain
              @
              resL_expand acts
        in
        Res (encode_proc p, resL_expand resL)

  let rec encode_prog ?(first=false) pi =
    let rec def_expand pi domain = begin
      let sep = if first then "_" else "," in
      match pi with
      | V.Def (_, [], _, _) -> pi
      | V.Def (k, x :: [], p, next_pi) ->
          begin match domain with
          | [] -> V.Proc V.Nil
          | n :: [] -> substitute_prog_var x n
              (V.Def (k ^ sep, [], p, next_pi))
          | n :: rest ->
              (V.Def (k ^ sep, [], p, def_expand pi rest)) |>
              substitute_prog_var x n
          end
      | V.Def (k, x :: params, p, next_pi) ->
          begin match domain with
          | [] -> V.Proc V.Nil
          | n :: [] -> substitute_prog_var x n
              (V.Def (k ^ sep, params, p, next_pi))
          | n :: rest ->
              (V.Def (k ^ sep, params, p, def_expand pi rest)) |>
              substitute_prog_var x n
          end
      | pi -> pi
    end
    in match pi with
    | V.Proc p -> Proc (encode_proc p)
    | V.Def (k, [], p, pi) -> Def (k, encode_proc p, encode_prog pi)
    | V.Def (k, params, p, pi) ->
        let expanded_def = def_expand
          (V.Def (k, params, p, pi))
          domain
        in
        encode_prog expanded_def

  let encode_file file =
    let pi = Vccs.Main.parse_file file in
    try encode_prog ~first:true pi
    with Failure msg ->
      let msg = Printf.sprintf "[!!] Evaluation error: %s\n" msg in
      Eval_error msg |> raise
end
