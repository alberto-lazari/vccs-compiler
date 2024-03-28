open Ast

let fprintf = Format.fprintf

let pp_act out a = match a with
  | Tau -> fprintf out "τ"
  | Input ch -> fprintf out "%s" ch
  | Output ch -> fprintf out "'%s" ch

let rec pp_proc out p = match p with
  | Nil -> fprintf out "0"
  | Act (a, p) -> fprintf out "%a. %a"
      pp_act a
      pp_proc p
  | Const k -> fprintf out "%s" k
  | Sum (p1, p2) -> fprintf out "@[<hov>(%a +@ %a)@]"
      pp_proc p1
      pp_proc p2
  | Paral (p1, p2) -> fprintf out "@[<hov>(%a |@ %a)@]"
      pp_proc p1
      pp_proc p2
  | Red (p, []) -> fprintf out "@[<hov>%a@]" pp_proc p
  | Red (p, fl) -> fprintf out "@[<hov>(%a[%a])@]"
      pp_proc p
      Format.(pp_print_list
        ~pp_sep: (fun out () -> fprintf out ",@ ")
        (fun out (a, b) ->
          fprintf out "%s/%s" a b)
      ) fl
  | Res (p, []) -> fprintf out "@[<hov>%a@]" pp_proc p
  | Res (p, cl) -> fprintf out "@[<hov>(%a \\ {%a})@]"
      pp_proc p
      Format.(pp_print_list
        ~pp_sep: (fun out () -> fprintf out ",@ ")
        Format.pp_print_string
      ) cl

let rec pp_prog out pi = match pi with
  | Proc p -> pp_proc out p
  | Def (k, p, pi) -> fprintf out
      "@[<hov>%s =@ %a;@ %a@]" k
      pp_proc p
      pp_prog pi
