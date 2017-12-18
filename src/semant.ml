(* Semantic checking for the MicroC compiler *)
open Ast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check_program program =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  let stmt_list =
    let stmts_as_items =
      List.filter (fun x -> match x with
        Ast.Stmt(x) -> true
        | _ -> false) program
    in List.map (fun x -> match x with
        Ast.Stmt(x) -> x
        | _ -> failwith "stmt casting didn't work") stmts_as_items
  in

  (*after you figure out which items are statements, you need to go through the statements
    and figure out which ones contain the variables*)
  let globals =
    let global_list = List.filter (fun x -> match x with
        Ast.VDecl(x) -> true
      | _ -> false) stmt_list
    in List.map (fun x -> match x with
        Ast.VDecl(x) -> x
      | _ -> failwith "not turned into global") global_list
  in

  let functions =
      let functions_as_items = List.filter (fun x -> match x with
          Ast.Function(x) -> true
        | _ -> false) program
      in
        let all_functions_as_items = functions_as_items
        in List.map (fun x -> match x with
            Ast.Function(x) -> x
          | _ -> failwith "function casting didn't work") all_functions_as_items
  in


  (* let function_locals =
    let get_locals_from_fbody fdecl =
      let get_vdecl locals_list stmt = match stmt with
          Ast.VDecl(typ, string) -> (typ, string) :: locals_list
          | _ -> locals_list
      in
      List.fold_left get_vdecl [] fdecl.Ast.body
    in List.fold_left get_locals_from_fbody (List.hd functions) (List.tl functions)
  in *)

  let symbols = List.fold_left (fun var_map (varType, varName) -> StringMap.add varName varType var_map)
    StringMap.empty (globals)
  in

  let type_of_identifier s =
    try StringMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Null, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  let built_in_decls =  StringMap.singleton "print"
     { typ = Null; fname = "print"; formals = [(String, "x")]; body = [] }
   in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    if List.mem "print" (List.map (fun fd -> fd.fname) functions)
      then raise (Failure ("function print may not be defined")) else ();

    if List.mem "println" (List.map (fun fd -> fd.fname) functions)
      then raise (Failure ("function println may not be defined")) else ();

      if List.mem "printf" (List.map (fun fd -> fd.fname) functions)
        then raise (Failure ("function printf may not be defined")) else ();

    report_duplicate (fun n -> "duplicate function " ^ n)
      (List.map (fun fd -> fd.fname) functions);

      if List.mem "main" (List.map (fun fd -> fd.fname) functions)
        then raise (Failure ("function main may not be defined")) else ();



    (* List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals; *)

    (* report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals); *)
  in

  let rec expr = function
      IntLit _ -> Int
    | BoolLit _ -> Bool
    | Id s -> type_of_identifier s
    | Assign(var, e) as ex -> let lt = type_of_identifier var
                              and rt = expr e in
      check_assign lt rt (Failure ("Illegal assignment: " ^ string_of_typ lt ^
           " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))
    | Binop(l, op, r) as e -> let t1 = expr l and t2 = expr r in
      (match op with
              Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
      | Equal | Neq when t1 = t2 -> Bool
      | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
      | And | Or when t1 = Bool && t2 = Bool -> Bool
            | _ -> raise (Failure ("illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e))
            )
    | Call(fname, actuals) as call -> let fd = function_decl fname in
       if List.length actuals != List.length fd.formals then
         raise (Failure ("expecting " ^ string_of_int
           (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
       else
         List.iter2 (fun (ft, _) e -> let et = expr e in
            ignore (check_assign ft et
              (Failure ("illegal actual argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
           fd.formals actuals;
         fd.typ
  in

  let check_stmt s = match s with
      VDecl _ -> ()
    | Expr e -> ignore (expr e)

  in




  (* Check for assignments and duplicate vdecls *)
  List.iter check_function functions;
  List.iter check_stmt stmt_list;
  report_duplicate (fun n -> "Duplicate assignment for " ^ n) (List.map snd globals);


(*
  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  (**** Checking Global Variables ****)
  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);
  (**** Checking Functions ****)
  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();
  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);
  (* Function declaration for a named function *)
  let built_in_decls =  StringMap.add "print"
     { typ = Void; fname = "print"; formals = [(Int, "x")];
       locals = []; body = [] } (*(StringMap.add "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] } (StringMap.singleton "printbig"
     { typ = Void; fname = "printbig"; formals = [(Int, "x")];
       locals = []; body = [] })) *)
   in
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in
  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  let _ = function_decl "main" in (* Ensure "main" is defined *)
  let check_function func =
    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;
    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);
    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;
    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);
    (* Return the type of an expression or throw an exception *)
    let rec expr = function
    	Literal _ -> Int
          | BoolLit _ -> Bool
          | Id s -> type_of_identifier s
          | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
    	(match op with
              Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
    	| Equal | Neq when t1 = t2 -> Bool
    	| Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
    	| And | Or when t1 = Bool && t2 = Bool -> Bool
            | _ -> raise (Failure ("illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e))
            )
          | Unop(op, e) as ex -> let t = expr e in
    	 (match op with
    	   Neg when t = Int -> Int
    	 | Not when t = Bool -> Bool
             | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
    	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
          | Noexpr -> Void
          | Assign(var, e) as ex -> let lt = type_of_identifier var
                                    and rt = expr e in
            check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
    				     " = " ^ string_of_typ rt ^ " in " ^
    				     string_of_expr ex))
          | Call(fname, actuals) as call -> let fd = function_decl fname in
             if List.length actuals != List.length fd.formals then
               raise (Failure ("expecting " ^ string_of_int
                 (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
             else
               List.iter2 (fun (ft, _) e -> let et = expr e in
                  ignore (check_assign ft et
                    (Failure ("illegal actual argument found " ^ string_of_typ et ^
                    " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
                 fd.formals actuals;
               fd.typ
    in
    let check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () in
    (* Verify a statement or throw an exception *)
    let rec stmt = function
	    Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in
    stmt (Block func.body)
*)
