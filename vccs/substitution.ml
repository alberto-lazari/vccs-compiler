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

let substitute_act_var x v a = match a with
  | Output (a, arg) -> Output (a, substitute_expr_var x v arg)
  | a -> a

let rec substitute_proc_var x v p = match p with
  | Nil -> Nil
  | Act (a, p) -> Act (substitute_act_var x v a, substitute_proc_var x v p)
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
