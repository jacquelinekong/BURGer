(*
 * Abstract Syntax Tree and functions for printing it
 * BURGer Programming Language
 * PLT Fall 2017
 * Authors:
 * Jacqueline Kong
 *)

(* Syntax Types *)
type typ = Char | String | Void
type bind = typ * string

type expr =
    Id of string
  | Call of string * expr list
  | IntLit of int
  | StringLit of string

type stmt =
    Expr of expr
  | Block of stmt list

(* type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
 } *)

 type program =
    stmt
 (* TODO: in real life this will be statements mixed in with fdecls, etc *)

(* Functions for Printing *)
(* let rec string_of_expr = function
    Literal(l) -> l
  | Id(s) -> s
  | String(s) -> s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")" *)
