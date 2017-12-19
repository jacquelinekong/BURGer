(* Abstract Syntax Tree
 * BURGer Programming Language
 * PLT Fall 2017
 * Authors:
 * Jacqueline Kong
 * Jordan Lee
 * Adrian Traviezo
 * Ashley Nguyen *)

(*** Syntax Types ***)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

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

type typ = Int | Bool | Char | String | Null | Array of typ * expr

type bind = typ * string

type stmt =
    Block of stmt list
  | Expr of expr
  | VDecl of bind
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | VAssign of bind * expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
 }

type item =
      Stmt of stmt
    | Function of func_decl

type program = item list

(*** Functions for Printing ***)

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Null -> "null"
  | Char -> "char"
  | String -> "string"

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"


let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(s) -> s
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  (* | Unop(o, e) -> string_of_uop o ^ string_of_expr e *)
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  (* | Noexpr -> "" *)

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  (* | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s *)
  (* | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2 *)
  (* | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s *)
  (* | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s *)
