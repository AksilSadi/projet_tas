open Tas_lib.Ast
open Tas_lib.Typer
open Tas_lib.Evaluator


let () =
  let t = Ref (Liste Empty) in
  Printf.printf "Term : %s\n" (print_term t);
  Printf.printf "expansif ? %b\n" (expansif t);
