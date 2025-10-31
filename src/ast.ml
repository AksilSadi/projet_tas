(* Termes *)
type pterm = Var of string | App of pterm * pterm | Abs of string * pterm | N of int | Add of pterm * pterm 
(* Types *) 
type ptype = Var of string | Arr of ptype * ptype | Nat 
(* Environnements de typage *) 
type env = (string * ptype) list 
(* Listes d'équations *) 
type equa = (ptype * ptype) list
(* zipper d'une liste d'équations *)
type equa_zip = equa * equa