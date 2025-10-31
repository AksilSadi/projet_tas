open Tas_lib.Ast
open Tas_lib.Typer
let ex_nat1 : pterm = App (Abs ("x", Add(Var "x", N 1)), N 3)
let inf_ex_nat1 : string = inference ex_nat1

let () = print_endline inf_ex_nat1

