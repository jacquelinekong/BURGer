(* BURGer Programming Language Semantic Checker *)

open Ast

module StringMap = Map.Make(String)

let check =

    (* Raise an exception if the given list has a duplicate *)
    let report_duplicate exceptf list =
      let rec helper = function
    n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
        | _ :: t -> helper t
        | [] -> ()
      in helper (List.sort compare list)
    in

    (* Raise an exception if a given binding is to a void type *)
    let check_not_void exceptf = function
        (Void, n) -> raise (Failure (exceptf n))
      | _ -> ()
    in

    let built_in_decls = StringMap.add "print" { typ = Void; fname = "print"; formals = [(String, "x")];
      locals = []; body = [] }
    in

    let check_function func =
      List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
        " in " ^ func.fname)) func.formals;

      report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
        (List.map snd func.formals);

      List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
        " in " ^ func.fname)) func.locals;

      report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
        (List.map snd func.locals);

      (* Type of each variable (global, formal, or local *)
      let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	      StringMap.empty (globals @ func.formals @ func.locals )
      in
      let type_of_identifier s =
        try StringMap.find s symbols
        with Not_found -> raise (Failure ("undeclared identifier " ^ s))


  in
  List.iter check_function functions
