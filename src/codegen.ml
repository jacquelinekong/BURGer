(*
 * Code generation for BURGer programming Language
 * Authors:
 * Jacqueline Kong
 * Adrian Traviezo
 * Jordan Lee
 * Ashley Nguyen
 *)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

(* passing in a program for hello world *)
let translate (program) = (* QUESTION: will we always only pass in a program bc we allow top-level code? *)
  let context = L.global_context () in
  let the_module = L.create_module context "BURGer"
  and i8_t = L.i8_type context
  (* and str_t = L.pointer_type (L.i8_type context) *)
  and void_t = L.void_type context in

  (* let ltype_of_typ = function
      A.Char -> i8_t
    | A.String -> str_t
    | A.Void -> void_t in *)

  (* printf() declaration *)
  let printf_t = L.var_arg_function_type i8_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

      let rec expr builder = function
            A.StringLit s -> L.build_global_stringptr s "str" builder
          | A.Call ("print", [s]) -> L.build_call printf_func [| (expr builder s) |] "print" builder in

      let rec stmt builder = function
            A.Expr e -> ignore(expr builder e); builder in

      let rec item builder = function
            A.Stmt st -> ignore(stmt builder st); builder in

      let prgm builder = ignore()

      let ftype = L.function_type void_t [| |] in
      (* Define main function so that we can have top-level code *)
      let funct = L.define_function "main" ftype the_module in
      let builder = L.builder_at_end context (L.entry_block funct) in
      item builder program;
      L.build_ret_void builder;
  the_module
