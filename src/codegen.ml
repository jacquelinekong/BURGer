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

(* START COPY from microc codegen *)
  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal var_map (formal_type, formal_name) param = L.set_value_name formal_name param;
	let local = L.build_alloca (ltype_of_typ formal_type) formal_name builder in
	ignore (L.build_store param local builder);
	StringMap.add formal_name local map in

      let add_local map (formal_type, formal_name) =
	let local_var = L.build_alloca (ltype_of_typ formal_type) formal_name builder
	in StringMap.add formal_name local_var map in

(* END COPY FROM MICROC *)

      let rec expr builder = function
            A.StringLit s -> L.build_global_stringptr s "str" builder
          | A.Call ("print", [s]) -> L.build_call printf_func [| (expr builder s) |] "print" builder in

      let stmt builder = function
            A.Expr e -> ignore(expr builder e); builder in

      let maintype = L.function_type void_t [| |] in
      (* Define main function so that we can have top-level code *)
      let main = L.define_function "main" maintype the_module in
      let builder = L.builder_at_end context (L.entry_block main) in
      stmt builder program;
      L.build_ret_void builder;
  the_module
