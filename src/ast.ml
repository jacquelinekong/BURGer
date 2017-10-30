(*
 * Abstract Syntax Tree and functions for printing it
 * BURGer Programming Language
 * PLT Fall 2017
 * Authors:
 * Jacqueline Kong
 *)

(* Syntax Types *)
type expr =
    Id of string
  | Call of string * expr list
  | Literal of int
  | String of string

type stmt =
    Expr of expr

(* Functions for Printing *)
let rec string_of_expr = function
    Literal(l) -> l
  | Id(s) -> s
  | String(s) -> s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
