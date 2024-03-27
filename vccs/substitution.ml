open Ast

let rec substitute_expr_var x v e = match e with
  | Num n -> Num n
  (* The actual meaningful step *)
  | Var id -> if id = x
      then Num v
      else Var id
  | AritBinop (op, e1, e2) ->
      let substitute_expr_var = substitute_expr_var x v in
      AritBinop (op, substitute_expr_var e1, substitute_expr_var e2)

let rec substitute_boolean_var x v b = match b with
  | Not b -> Not (substitute_boolean_var x v b)
  | And (b1, b2) ->
      let substitute_boolean_var = substitute_boolean_var x v in
      And (substitute_boolean_var b1, substitute_boolean_var b2)
  | Or (b1, b2) ->
      let substitute_boolean_var = substitute_boolean_var x v in
      Or (substitute_boolean_var b1, substitute_boolean_var b2)
  | BoolBinop (op, e1, e2) ->
      let substitute_expr_var = substitute_expr_var x v in
      BoolBinop (op, substitute_expr_var e1, substitute_expr_var e2)
  | b -> b

let rec substitute_proc_var x v p = match p with
  | Nil -> Nil
  | Act (a, p) -> begin match a with
      | Tau -> Act (Tau, substitute_proc_var x v p)
      | Input (_, var) -> let nextP =
          if var = x
            (* Allow variable shadowing *)
            then p
            else substitute_proc_var x v p
          in Act (a, nextP)
      | Output (ch, arg) -> Act (Output (ch, substitute_expr_var x v arg), substitute_proc_var x v p)
      end
  | Const (k, args) -> Const (k, List.map (substitute_expr_var x v) args)
  | If (b, p) -> If (substitute_boolean_var x v b, substitute_proc_var x v p)
  | Sum (p1, p2) ->
      let substitute_proc_var = substitute_proc_var x v in
      Sum (substitute_proc_var p1, substitute_proc_var p2)
  | Paral (p1, p2) ->
      let substitute_proc_var = substitute_proc_var x v in
      Paral (substitute_proc_var p1, substitute_proc_var p2)
  | Red (p, fs) -> Red (substitute_proc_var x v p, fs)
  | Res (p, resL) -> Res (substitute_proc_var x v p, resL)

let substitute_prog_var x v pi = match pi with
  | Def (k, params, p, pi) ->
      let k_n = k ^ (string_of_int v) in
      Def (k_n, params, substitute_proc_var x v p, pi)
  | pi -> pi
