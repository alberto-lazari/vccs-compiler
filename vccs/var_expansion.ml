open Ast
open Var_substitution

let ch_n ch n = Printf.sprintf "%s_%d" ch n

let rec expand_f_var domain f = match f with
  | [] -> []
  | (a, b) :: rest ->
      let expanded_red =
        List.map (fun n -> (ch_n a n, ch_n b n)) domain
      in
      expanded_red @ (expand_f_var domain rest)

let rec expand_resL_var domain l = match l with
  | [] -> []
  | a :: acts ->
      let expanded_ch =
        List.map (fun n -> ch_n a n) domain
      in
      expanded_ch @ (expand_resL_var domain acts)

let rec expand_prog_var domain first pi =
  let sep = if first then "_" else "," in
  match pi with
  | Def (_, [], _, _) -> pi
  | Def (k, x :: params, p, next_pi) ->
      begin match domain with
      | [] -> Proc Nil
      | n :: [] -> substitute_prog_var x n
          (Def (k ^ sep, params, p, next_pi))
      | n :: rest ->
          (Def (k ^ sep, params, p, expand_prog_var rest first pi)) |>
          substitute_prog_var x n
      end
  | pi -> pi
