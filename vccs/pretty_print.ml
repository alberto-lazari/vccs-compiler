open Ast

let fprintf = Format.fprintf

let pp_abop out op = match op with
  | Add -> fprintf out "+"
  | Sub -> fprintf out "-"
  | Mult -> fprintf out "*"
  | Div -> fprintf out "/"
  | Mod -> fprintf out "mod"

let rec pp_expr out e = match e with
  | Int n -> fprintf out "%d" n
  | Var x -> fprintf out "%s" x
  | AritBinop (op, e1, e2) -> fprintf out "@[<hov>(%a %a@ %a)@]"
      pp_expr e1
      pp_abop op
      pp_expr e2

let pp_bbop out op = match op with
  | Eq -> fprintf out "="
  | Neq -> fprintf out "≠"
  | Lt -> fprintf out "<"
  | Gt -> fprintf out ">"
  | Leq -> fprintf out "≤"
  | Geq -> fprintf out "≥"

let rec pp_boolean out b = match b with
  | True -> fprintf out "true"
  | False -> fprintf out "false"
  | Not b -> fprintf out "not %a"
      pp_boolean b
  | And (b1, b2) -> fprintf out "(%a and@ %a)"
      pp_boolean b1
      pp_boolean b2
  | Or (b1, b2) -> fprintf out "(%a or@ %a)"
      pp_boolean b1
      pp_boolean b2
  | BoolBinop (op, e1, e2) -> fprintf out "(%a %a@ %a)"
      pp_expr e1
      pp_bbop op
      pp_expr e2

let pp_act out a = match a with
  | Tau -> fprintf out "τ"
  | Input (ch, x) -> fprintf out "%s(%s)" ch x
  | Output (ch, e) -> fprintf out "'%s(%a)" ch
      pp_expr e

let rec pp_proc out p = match p with
  | Nil -> fprintf out "0"
  | Act (a, p) -> fprintf out "%a. %a"
      pp_act a
      pp_proc p
  | Const (k, []) -> fprintf out "%s" k
  | Const (k, args) -> fprintf out "@[<hov>%s(%a)@]" k
      Format.(pp_print_list
        ~pp_sep: (fun out () -> fprintf out ",@ ")
        pp_expr
      ) args
  | If (b, p) -> fprintf out
      "@[<hov>if@ %a@ then@ %a@]"
      pp_boolean b
      pp_proc p
  | Sum (p1, p2) -> fprintf out "@[<hov>(%a +@ %a)@]"
      pp_proc p1
      pp_proc p2
  | Paral (p1, p2) -> fprintf out "@[<hov>(%a |@ %a)@]"
      pp_proc p1
      pp_proc p2
  | Red (p, fl) -> fprintf out "@[<hov>(%a[%a])@]"
      pp_proc p
      Format.(pp_print_list
        ~pp_sep: (fun out () -> fprintf out ",@ ")
        (fun out (a, b) ->
          fprintf out "%s/%s" a b)
      ) fl
  | Res (p, cl) -> fprintf out "@[<hov>(%a \\ {%a})@]"
      pp_proc p
      Format.(pp_print_list
        ~pp_sep: (fun out () -> fprintf out ",@ ")
        Format.pp_print_string
      ) cl

let rec pp_prog out pi = match pi with
  | Proc (p) -> pp_proc out p
  | Def (k, [], p, pi) -> fprintf out
      "@[<hov>%s =@ %a;@ %a@]" k
      pp_proc p
      pp_prog pi
  | Def (k, params, p, pi) -> fprintf out
      "@[<hov>%s(%a) =@ %a;@ %a@]" k
      Format.(pp_print_list
        ~pp_sep: (fun out () -> fprintf out ",@ ")
        Format.pp_print_string
      ) params
      pp_proc p
      pp_prog pi
