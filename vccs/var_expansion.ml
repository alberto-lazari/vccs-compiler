open Ast
open Var_substitution

let ch_n ch ?(sep="_")  n = Printf.sprintf "%s%s%d" ch sep n

(*
  Expand input-bound variables for each value in domain:
  let domain = {min..max}

  a(x). P(x) -> a_min. P(min) + a_min+1. P(min+1)  + ... + a_max. P(max)
*)
let rec expand_input_var domain p input =
  let (ch, x) = input in
  let a n = Act
    (Input (ch_n ch n, x), substitute_proc_var x n p)
  in
  match domain with
  | [] -> Nil
  | n :: [] -> a n
  | n :: rest -> Sum (a n, expand_input_var rest p input)

(*
  Expand redirected channels for each value in domain:
  let domain = {min..max}

  P[a/b] -> P[a_min/b_min, a_min+1/b_min+1, ..., a_max/b_max]
*)
let rec expand_f_var domain f = match f with
  | [] -> []
  | (a, b) :: rest ->
      let expanded_red =
        List.map (fun n -> (ch_n a n, ch_n b n)) domain
      in
      expanded_red @ (expand_f_var domain rest)

(*
  Expand restricted channels for each value in domain:
  let domain = {min..max}

  P \ a -> P \ {a_min, a_min+1, ..., a_max}
*)
let rec expand_resL_var domain l = match l with
  | [] -> []
  | a :: acts ->
      let expanded_ch =
        List.map (fun n -> ch_n a n) domain
      in
      expanded_ch @ (expand_resL_var domain acts)

(*
  Expand process definition parameters for each value in domain:
  let domain = {min..max}

  P(x) = proc(x); pi ->
  P_min = proc(min); P_min+1 = proc(min+1); ...; P_max = proc(max); pi
*)
let rec expand_prog_var domain first pi =
  let sep = if first then "_" else "," in
  match pi with
  | Def (_, [], _, _) -> pi
  | Def (k, x :: params, p, next_pi) ->
      begin match domain with
      | [] -> Proc Nil
      | n :: [] ->
          let p = substitute_proc_var x n p in
          Def (ch_n k ~sep:sep n, params, p, next_pi)
      | n :: rest ->
          let p = substitute_proc_var x n p in
          Def (ch_n k ~sep:sep n, params, p, expand_prog_var rest first pi)
      end
  | pi -> pi
