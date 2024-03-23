type act =
  | Tau
  | Input of string
  | Output of string

type proc =
  | Nil
  | Act of act * proc
  | Const of string
  | Sum of proc * proc
  | Paral of proc * proc
  | Red of proc * (string * string) list
  | Res of proc * string list

type prog =
  | Proc of proc
  | Def of string * proc * prog
