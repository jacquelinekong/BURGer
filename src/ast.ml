(* Abstract Syntax Tree
 * BURGer Programming Language
 * PLT Fall 2017
 * Authors:
 * Jacqueline Kong
 * Jordan Lee
 * Adrian Traviezo
 * Ashley Nguyen *)

(* Syntax Types *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Int | Bool | Char | String | Null

type bind = typ * string

type expr =
    Id of string
  | Call of string * expr list
  | IntLit of int
  | BoolLit of bool
  | StringLit of string
  | Assign of string * expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  | NoExpr

type stmt =
    Expr of expr
  | VDecl of bind
  | Return of expr
  | If of expr * stmt list * stmt list

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
 }

type item =
      Stmt of stmt
    | Function of func_decl

type program = item list

 (* TODO: in real life this will be statements mixed in with fdecls, etc *)

(* Functions for Printing *)
(* let rec string_of_expr = function
    Literal(l) -> l
  | Id(s) -> s
  | String(s) -> s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")" *)
