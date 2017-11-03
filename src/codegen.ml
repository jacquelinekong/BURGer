(*
 * Code generation for BURGer programming Language
 * Authors:
 * Jacqueline Kong
 *)

Module L = Llvm
Module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "BURGer"
  and i8_t = L.i8_type context
  and str_t = L.pointer_type (L.i8_type context) in

  let ltype_of_typ = function
      A.Char -> i8_t
    | A.String -> str_t in

  (* printf() declaration *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

      let rec expr builder = function
            A.String s -> L.build_global_stringptr s "str" builder
          | A.Call ("print", [s]) -> L.build_call printf_func s "print" builder in

      let stmt builder = function
            A.Expr e -> ignore(expr builder e); builder in

  the_module


  (* NOTE: what types do we have, and how to encode those in LLVM?
   * LLVM has pretty much all the types you need to implement C
   * to figure out what your codegen needs to do, take a small example in your language
   * and try to code it in C. If you can figure the types you need to do it in C, then it's
   * a small jump to figure out how to do it in LLVM *)
