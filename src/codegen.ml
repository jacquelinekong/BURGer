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

(* external print_s: s -> unit = "caml_print_string"
external print_i: i -> unit = "caml_print_int" *)


(* passing in a program for hello world *)
let translate (program) = (* QUESTION: will we always only pass in a program bc we allow top-level code? *)
  let context = L.global_context () in
    let the_module = L.create_module context "BURGer"
    and i8_t = L.i8_type context
    and str_t = L.pointer_type (L.i8_type context)
    and null_t = L.void_type context
    and i1_t   = L.i1_type context
    and i32_t  = L.i32_type context in

  let ltype_of_typ = function
      A.Char -> i8_t
    | A.String -> str_t
    | A.Null -> null_t
    | A.Int -> i32_t
    | A.Bool -> i1_t
  in

  let stmt_list =
    let stmts_as_items =
      List.filter (fun x -> match x with
        A.Stmt(x) -> true
        | _ -> false) program
    in List.map (fun x -> match x with
        A.Stmt(x) -> x
        | _ -> failwith "stmt casting didn't work") stmts_as_items
  in

  (*after you figure out which items are statements, you need to go through the statements
    and figure out which ones contain the variables*)
  let globals =
    let global_list = List.filter (fun x -> match x with
        A.VDecl(x) -> true
      | _ -> false) stmt_list
    in List.map (fun x -> match x with
        A.VDecl(x) -> x
      | _ -> failwith "not turned into global") global_list
  in

  let functions =
    let fdecl_main = A.Function({
           typ = A.Int;
           fname = "main";
           formals = [];
           body = List.rev(A.Return(A.IntLit(0)) :: List.rev(stmt_list))
         })
    in
      let functions_as_items = List.filter (fun x -> match x with
          A.Function(x) -> true
        | _ -> false) program
      in
        let all_functions_as_items = fdecl_main :: functions_as_items
        in List.map (fun x -> match x with
            A.Function(x) -> x
          | _ -> failwith "function casting didn't work") all_functions_as_items
  in

  (*after you figure out which items are statements, you need to go through the statements
    and figure out which ones contain the variables*)
  let global_vars =
  let global_var map (t, n) =
    let init = L.const_int (ltype_of_typ t) 0
    in StringMap.add n (L.define_global n init the_module) map in
  List.fold_left global_var StringMap.empty globals in

  (* printf() declaration *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl map fdecl =
      let name = fdecl.A.fname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) map
    in
    List.fold_left function_decl StringMap.empty functions
  in

    (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    let local_vars =
      let add_formal var_map (formal_type, formal_name) param = L.set_value_name formal_name param;
      	let local = L.build_alloca (ltype_of_typ formal_type) formal_name builder in
      	ignore (L.build_store param local builder);
        StringMap.add formal_name local var_map
      in

      let add_local map (formal_type, formal_name) =
        let local_var = L.build_alloca (ltype_of_typ formal_type) formal_name builder in
        StringMap.add formal_name local_var map
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in

      let function_locals =
        let get_locals_from_fbody function_body =
          let get_vdecl locals_list stmt = match stmt with
              A.VDecl(typ, string) -> (typ, string) :: locals_list
              | _ -> locals_list
          in
          List.fold_left get_vdecl [] function_body
        in get_locals_from_fbody fdecl.A.body
      in List.fold_left add_local formals function_locals
    in

  let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
  in

      let rec expr builder = function
        A.StringLit e -> L.build_global_stringptr e "str" builder
      | A.Call ("print", [s]) ->
        let test = s in (match s with
          A.Id test -> L.build_call printf_func [| (expr builder s) |] "print" builder
        | A.StringLit test -> L.build_call printf_func [| (expr builder s) |] "print" builder
        | A.IntLit test -> L.build_call printf_func [| int_format_str ; (expr builder s) |]
                             "print" builder
          )
        (* | A.Call ("print_int", [s]) ->  L.build_call printf_func [| int_format_str ; (expr builder s) |]
           "print" builder

           | A.BoolLit test -> L.build_call printf_func [| int_format_str ; (expr builder s) |] "print" builder
        *)

      (* | A.Call ("print", [s]) -> L.build_call printf_func [| (expr builder s) |] "print" builder
      | A.Call ("print_int", [s]) ->  L.build_call printf_func [| int_format_str ; (expr builder s) |]
                                        "print" builder *)

        (* let int_format_str builder = L.build_global_stringptr "%d\n" "fmt" llbuilder;
        and str_format_str builder = L.build_global_stringptr "%s\n" "fmt" llbuilder in

        let format_str s_typ builder = match s_typ with
            A.Int -> int_format_str builder
          | A.String -> str_format_str builder
          | _ -> raise (Failure "Invalid printf type")
        in

        let e' = expr builder e
        and e_type =
        L.build_call printf_func [| format_str e_type llbuilder; e' |]
          "printf" builder *)

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
    	  | A.And     -> L.build_and
    	  | A.Or      -> L.build_or
    	  | A.Equal   -> L.build_icmp L.Icmp.Eq
    	  | A.Neq     -> L.build_icmp L.Icmp.Ne
    	  | A.Less    -> L.build_icmp L.Icmp.Slt
    	  | A.Leq     -> L.build_icmp L.Icmp.Sle
    	  | A.Greater -> L.build_icmp L.Icmp.Sgt
    	  | A.Geq     -> L.build_icmp L.Icmp.Sge
    	  ) e1' e2' "tmp" builder
      (* | A.Unop(op, e) ->
    	  let e' = expr builder e in
      	  (match op with
      	    A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder *)
      | A.Assign (s, e) -> let e' = expr builder e in
    	                   ignore (L.build_store e' (lookup s) builder); e'
      | A.Call (f, act) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
  	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
  	 let result = (match fdecl.A.typ with A.Null -> ""
                                              | _ -> f ^ "_result") in
           L.build_call fdef (Array.of_list actuals) result builder
  in

  (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	     Some _ -> ()
      | None -> ignore (f builder)
    in

    let rec stmt builder = function
      A.Block sl -> List.fold_left stmt builder sl
    |  A.Expr e -> ignore(expr builder e); builder
    | A.VDecl (typ, string) -> builder
    | A.Return e -> ignore (match fdecl.A.typ with
        A.Null -> L.build_ret_void builder
        | _ -> L.build_ret (expr builder e) builder); builder
    | A.If (predicate, then_stmt, else_stmt) ->
       let bool_val = expr builder predicate in
    	 let merge_bb = L.append_block context "merge" the_function in

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

      (* | A.For (e1, e2, e3, body) -> stmt builder ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] ) *)
  in
      (* let ftype = L.function_type null_t [| |] in
      (* Define main function so that we can have top-level code *)
      let funct = L.define_function "main" ftype the_module in
      let builder = L.builder_at_end context (L.entry_block funct) in
      (* List.iter item builder program; *)
      L.build_ret_void builder; (*List.iter buildprogrambody items; this is one of the first functions we define*)
  in the_module  *)
  (* let rec item builder = function
      A.Stmt s -> ignore(stmt builder s); builder
    | A.Function f -> build_function_body f
  in *)
   (* Build the code for each statement in the function *)
  let builder = stmt builder (A.Block fdecl.A.body) in

     (* Add a return if the last block falls off the end *)
     add_terminal builder (match fdecl.A.typ with
         A.Null -> L.build_ret_void
       | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

List.iter build_function_body functions;
the_module
