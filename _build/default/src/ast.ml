type 'a liste = Empty | Cons of 'a *'a liste 
(* Termes *)
type pterm = Var of string 
| App of pterm * pterm 
| Abs of string * pterm 
| N of int
| Liste of pterm liste
| Add of pterm * pterm 
| Ifz of pterm * pterm * pterm 
| Succ of pterm 
| Pred of pterm
| Couple of pterm * pterm
| ProdG of pterm
| ProdD of pterm
| SumG of pterm
| SumD of pterm
| MatchSum of pterm * string * pterm * string * pterm
| Let of string * pterm * pterm              
| Fix of pterm                      
| Hd of pterm                                
| Tl of pterm                                
| IfEmpty of pterm * pterm * pterm           
(* correspond à sw M ▷ x1 -> N1 | x2 -> N2 *)
(* Types *)
type ptype = Var of string 
| Arr of ptype * ptype 
| Nat
| Prod of ptype * ptype
| Sum of ptype * ptype
| List of ptype  (* [T] pour les listes *)
| Forall of string list * ptype
(* Environnements de typage *) 
type env = (string * ptype) list 
(* Listes d'équations *) 
type equa = (ptype * ptype) list
(* zipper d'une liste d'équations *)
type equa_zip = equa * equa