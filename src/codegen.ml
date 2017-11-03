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
  (* TODO: implement string type *)
  (* and string = *)
  in

  (* NOTE: what types do we have, and how to encode those in LLVM?
   * LLVM has pretty much all the types you need to implement C
   * to figure out what your codegen needs to do, take a small example in your language
   * and try to code it in C. If you can figure the types you need to do it in C, then it's
   * a small jump to figure out how to do it in LLVM *)
