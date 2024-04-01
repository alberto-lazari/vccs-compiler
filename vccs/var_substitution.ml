(* Substitute an expression's free variable with a value *)
open Ast

let rec sub_expr x v e = match e with
  | Num n -> Num n
  (* The actual meaningful step *)
  | Var id -> if id = x
      then Num v
      else Var id
  | AritBinop (op, e1, e2) ->
      let sub_expr = sub_expr x v in
      AritBinop (op, sub_expr e1, sub_expr e2)

let rec sub_boolean x v b =
  let sub = sub_boolean x v in
  match b with
  | Not b -> Not (sub b)
  | And (b1, b2) -> And (sub b1, sub b2)
  | Or (b1, b2) -> Or (sub b1, sub b2)
  | BoolBinop (op, e1, e2) -> BoolBinop (op, sub_expr x v e1, sub_expr x v e2)
  | b -> b

let rec sub_proc x v p = match p with
  | Nil -> Nil
  | Act (a, p) -> begin match a with
      | Tau -> Act (Tau, sub_proc x v p)
      | Input (_, var) -> let nextP =
          if var = x
            (* Allow variable shadowing *)
            then p
            else sub_proc x v p
          in Act (a, nextP)
      | Output (ch, arg) -> Act (Output (ch, sub_expr x v arg), sub_proc x v p)
      end
  | Const (k, args) -> Const (k, List.map (sub_expr x v) args)
  | If (b, p) -> If (sub_boolean x v b, sub_proc x v p)
  | Sum (p1, p2) -> Sum (sub_proc x v p1, sub_proc x v p2)
  | Paral (p1, p2) -> Paral (sub_proc x v p1, sub_proc x v p2)
  | Red (p, fs) -> Red (sub_proc x v p, fs)
  | Res (p, resL) -> Res (sub_proc x v p, resL)
