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
    and str_t = L.pointer_type (L.i8_type context)
    and null_t = L.void_type context
    and i1_t   = L.i1_type context
    and i32_t  = L.i32_type context

  in

  let ltype_of_typ = function
      A.Char -> i8_t
    | A.String -> str_t
    | A.Null -> null_t
    | A.Int -> i32_t
    | A.Bool -> i1_t
  in

  (* printf() declaration *)
  let printf_t = L.var_arg_function_type i8_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

      let rec expr builder = function
        A.StringLit s -> L.build_global_stringptr s "str" builder
      | A.Call ("print", [s]) -> L.build_call printf_func [| (expr builder s) |] "print" builder
      | A.IntLit i -> L.const_int i32_t i
          | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
          | A.NoExpr -> L.const_int i32_t 0
          | A.Id s -> L.build_load (lookup s) s builder
          | A.Binop (e1, op, e2) ->
    	  let e1' = expr builder e1
    	  and e2' = expr builder e2 in
    	  (match op with
    	    A.Add     -> L.build_add
    	  | A.Sub     -> L.build_sub
    	  | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
    	  (* | A.And     -> L.build_and
    	  | A.Or      -> L.build_or
    	  | A.Equal   -> L.build_icmp L.Icmp.Eq
    	  | A.Neq     -> L.build_icmp L.Icmp.Ne
    	  | A.Less    -> L.build_icmp L.Icmp.Slt
    	  | A.Leq     -> L.build_icmp L.Icmp.Sle
    	  | A.Greater -> L.build_icmp L.Icmp.Sgt
    	  | A.Geq     -> L.build_icmp L.Icmp.Sge *)
    	  ) e1' e2' "tmp" builder
          (* | A.Unop(op, e) ->
    	  let e' = expr builder e in
      	  (match op with
      	    A.Neg     -> L.build_neg
            | A.Not     -> L.build_not) e' "tmp" builder *)
          (* | A.Assign (s, e) -> let e' = expr builder e in
    	                   ignore (L.build_store e' (lookup s) builder); e' *)
          | A.Call (f, act) ->
             let (fdef, fdecl) = StringMap.find f function_decls in
    	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
    	 let result = (match fdecl.A.typ with A.Void -> ""
                                                | _ -> f ^ "_result") in
             L.build_call fdef (Array.of_list actuals) result builder
  in

      let rec stmt builder = function
        A.Expr e -> ignore(expr builder e); builder in

        (* let merge_bb = L.append_block context "merge" the_function in

  	 let then_bb = L.append_block context "then" the_function in
  	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
  	   (L.build_br merge_bb);

  	 let else_bb = L.append_block context "else" the_function in
  	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
  	   (L.build_br merge_bb);

  	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
  	 L.builder_at_end context merge_bb

        | A.While (predicate, body) ->
  	  let pred_bb = L.append_block context "while" the_function in
  	  ignore (L.build_br pred_bb builder);

  	  let body_bb = L.append_block context "while_body" the_function in
  	  add_terminal (stmt (L.builder_at_end context body_bb) body)
  	    (L.build_br pred_bb);

  	  let pred_builder = L.builder_at_end context pred_bb in
  	  let bool_val = expr pred_builder predicate in

  	  let merge_bb = L.append_block context "merge" the_function in
  	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
  	  L.builder_at_end context merge_bb

        | A.For (e1, e2, e3, body) -> stmt builder
  	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
      in *)

      let rec item builder = function
            A.Stmt st -> ignore(stmt builder st); builder in

      let prgm builder = ignore() in

      let ftype = L.function_type null_t [| |] in
      (* Define main function so that we can have top-level code *)
      let funct = L.define_function "main" ftype the_module in
      let builder = L.builder_at_end context (L.entry_block funct) in
      item builder program;
      L.build_ret_void builder; (*List.iter buildprogrambody items; this is one of the first functions we define*)
      the_module
