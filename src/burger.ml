(* Top-Level of the BURGer Programming Language
* Author:
*   Jacqueline Kong
*)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  Semant.check_program ast;
  let m = Codegen.translate ast in
  Llvm_analysis.assert_valid_module m;
  print_string (Llvm.string_of_llmodule m)
