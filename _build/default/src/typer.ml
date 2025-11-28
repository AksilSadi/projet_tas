open Ast

(* pretty printer de termes*)     
let rec print_term (t : pterm) : string =
  match t with
    Var x -> x
    | App (t1, t2) -> "(" ^ (print_term t1) ^" "^ (print_term t2) ^ ")"
    | Abs (x, t) -> "(fun "^ x ^" -> " ^ (print_term t) ^")" 
    | N n -> string_of_int n
    | Add (t1, t2) -> "(" ^ (print_term t1) ^" + "^ (print_term t2) ^ ")"
    | Ifz (t1, t2, t3) -> "(ifz " ^ (print_term t1) ^ " then " ^ (print_term t2) ^ " else " ^ (print_term t3) ^ ")"
    | Succ t1 -> "(succ " ^ (print_term t1) ^ ")"
    | Pred t1 -> "(pred " ^ (print_term t1) ^ ")"
    | Couple (t1, t2) -> "(" ^ (print_term t1) ^", "^ (print_term t2) ^ ")"
    | ProdG t1 -> "(π₁ " ^ (print_term t1) ^ ")"
    | ProdD t1 -> "(π₂ " ^ (print_term t1) ^ ")"
    | SumG t1 -> "(inl " ^ (print_term t1) ^ ")"
    | SumD t1 -> "(inr " ^ (print_term t1) ^ ")"
    | MatchSum (t0, x1, t1, x2, t2) -> "(match " ^ (print_term t0) ^ " with inl " ^ x1 ^ " -> " ^ (print_term t1) ^ " | inr " ^ x2 ^ " -> " ^ (print_term t2) ^ ")"

(* pretty printer de types*)                    
let rec print_type (t : ptype) : string =
  match t with
    Var x -> x
  | Arr (t1, t2) -> "(" ^ (print_type t1) ^" -> "^ (print_type t2) ^")"
  | Prod (t1, t2) -> "(" ^ (print_type t1) ^" × "^ (print_type t2) ^")"
  | Sum (t1, t2) -> "(" ^ (print_type t1) ^" + "^ (print_type t2) ^")"
  | Nat -> "Nat" 
(* générateur de noms frais de variables de types *)
let compteur_var : int ref = ref 0                    

let nouvelle_var () : string = compteur_var := !compteur_var + 1; 
  "T"^(string_of_int !compteur_var)


exception VarPasTrouve

(* cherche le type d'une variable dans un environnement *)
let rec cherche_type (v : string) (e : env) : ptype =
  match e with
    [] -> raise VarPasTrouve
  | (v1, t1)::_ when v1 = v -> t1
  | (_, _):: q -> (cherche_type v q) 

(* vérificateur d'occurence de variables *)  
let rec appartient_type (v : string) (t : ptype) : bool =
  match t with
    Var v1 when v1 = v -> true
  | Arr (t1, t2) -> (appartient_type v t1) || (appartient_type v t2) 
  | Prod (t1, t2) -> (appartient_type v t1) || (appartient_type v t2)
  | Sum (t1, t2) -> (appartient_type v t1) || (appartient_type v t2)
  | _ -> false

(* remplace une variable par un type dans type *)
let rec substitue_type (t : ptype) (v : string) (t0 : ptype) : ptype =
  match t with
    Var v1 when v1 = v -> t0
  | Var v2 -> Var v2
  | Arr (t1, t2) -> Arr (substitue_type t1 v t0, substitue_type t2 v t0) 
  | Prod (t1, t2) -> Prod (substitue_type t1 v t0, substitue_type t2 v t0)
  | Sum (t1, t2) -> Sum (substitue_type t1 v t0, substitue_type t2 v t0)
  | Nat -> Nat 

(* remplace une variable par un type dans une liste d'équations*)
let substitue_type_partout (e : equa) (v : string) (t0 : ptype) : equa =
  List.map (fun (x, y) -> (substitue_type x v t0, substitue_type y v t0)) e

(* genere des equations de typage à partir d'un terme *)  
let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa =
  match te with 
    Var v -> let tv : ptype = cherche_type v e in [(ty, tv)] 
  | App (t1, t2) -> let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Arr (Var nv, ty)) e in
      let eq2 : equa = genere_equa t2 (Var nv) e in
      eq1 @ eq2
  | Abs (x, t) -> let nv1 : string = nouvelle_var () 
      and nv2 : string = nouvelle_var () in
      (ty, Arr (Var nv1, Var nv2))::(genere_equa t (Var nv2) ((x, Var nv1)::e))  
  | N _ -> [(ty, Nat)]
  | Add (t1, t2) -> let eq1 : equa = genere_equa t1 Nat e in
      let eq2 : equa = genere_equa t2 Nat e in
      (ty, Nat)::(eq1 @ eq2)
  | Ifz (t1, t2, t3) -> let eq1 : equa = genere_equa t1 Nat e in
      let eq2 : equa = genere_equa t2 ty e in
      let eq3 : equa = genere_equa t3 ty e in
      eq1 @ eq2 @ eq3
  | Succ t1 -> let eq1 : equa = genere_equa t1 Nat e in
      (ty, Nat)::eq1
  | Pred t1 -> let eq1 : equa = genere_equa t1 Nat e in
      (ty, Nat)::eq1
  | Couple (t1, t2) -> let nv1 : string = nouvelle_var () 
      and nv2 : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Var nv1) e in
      let eq2 : equa = genere_equa t2 (Var nv2) e in
      (ty, Prod (Var nv1, Var nv2))::(eq1 @ eq2)
  | ProdG t1 -> let nv1 : string = nouvelle_var () 
      and nv2 : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Prod (Var nv1, Var nv2)) e in
      (ty, Var nv1)::eq1
  | ProdD t1 -> let nv1 : string = nouvelle_var () 
      and nv2 : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Prod (Var nv1, Var nv2)) e in
      (ty, Var nv2)::eq1
  | SumG t1 -> let nv1 : string = nouvelle_var () 
      and nv2 : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Var nv1) e in
      (ty, Sum (Var nv1, Var nv2))::eq1
  | SumD t1 -> let nv1 : string = nouvelle_var () 
      and nv2 : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Var nv2) e in
      (ty, Sum (Var nv1, Var nv2))::eq1
  | MatchSum (t0, x1, t1, x2, t2) -> let nv1 : string = nouvelle_var () 
      and nv2 : string = nouvelle_var () 
      and nv_sum : string = nouvelle_var () in
      let eq0 = genere_equa t0 (Sum (Var nv1, Var nv2)) e in
      let eq1 = genere_equa t1 (Var nv_sum) ((x1, Var nv1)::e) in
      let eq2 = genere_equa t2 (Var nv_sum) ((x2, Var nv2)::e) in
      (ty, Var nv_sum) :: (eq0 @ eq1 @ eq2)
      
exception Echec_unif of string 
(* rembobine le zipper *)
let rembobine (e : equa_zip) =
  match e with
    ([], _) -> e
  | (c::e1, e2) -> (e1, c::e2)

(* remplace unee variable par un type dans un zipper d'équations *)
let substitue_type_zip (e : equa_zip) (v : string) (t0 : ptype) : equa_zip =
  match e with
    (e1, e2) -> (substitue_type_partout e1 v t0, substitue_type_partout e2 v t0)

(* trouve un type associé à une variable dans un zipper d'équation *)
let rec trouve_but (e : equa_zip) (but : string) = 
  match e with
    (_, []) -> raise VarPasTrouve
  | (_, (Var v, t)::_) when v = but -> t
  | (_, (t, Var v)::_) when v = but -> t 
  | (e1, c::e2) -> trouve_but (c::e1, e2) but 
                     
(* résout un système d'équations *) 
let rec unification (e : equa_zip) (but : string) : ptype = 
  match e with 
    (* on a passé toutes les équations : succes *)
    (_, []) -> (try trouve_but (rembobine e) but with VarPasTrouve -> raise (Echec_unif "but pas trouvé"))
    (* equation avec but : on passe *)
  | (e1, (Var v1, t2)::e2) when v1 = but ->  unification ((Var v1, t2)::e1, e2) but
    (* deux variables : remplacer l'une par l'autre *)
  | (e1, (Var v1, Var v2)::e2) ->  unification (substitue_type_zip (rembobine (e1,e2)) v2 (Var v1)) but
    (* une variable à gauche : vérification d'occurence puis remplacement *)
  | (e1, (Var v1, t2)::e2) ->  if appartient_type v1 t2 then raise (Echec_unif ("occurence de "^ v1 ^" dans "^(print_type t2))) else  unification (substitue_type_zip (rembobine (e1,e2)) v1 t2) but
    (* une variable à droite : vérification d'occurence puis remplacement *)
  | (e1, (t1, Var v2)::e2) ->  if appartient_type v2 t1 then raise (Echec_unif ("occurence de "^ v2 ^" dans " ^(print_type t1))) else  unification (substitue_type_zip (rembobine (e1,e2)) v2 t1) but 
    (* types fleche des deux cotes : on decompose  *)
  | (e1, (Arr (t1,t2), Arr (t3, t4))::e2) -> unification (e1, (t1, t3)::(t2, t4)::e2) but 
    (* types fleche à gauche pas à droite : echec  *)
  | (_, (Arr (_,_), t3)::_) -> raise (Echec_unif ("type fleche non-unifiable avec "^(print_type t3)))     
    (* types fleche à droite pas à gauche : echec  *)
  | (_, (t3, Arr (_,_))::_) -> raise (Echec_unif ("type fleche non-unifiable avec "^(print_type t3)))     
    (* types nat des deux cotes : on passe *)
  | (e1, (Nat, Nat)::e2) -> unification (e1, e2) but
  | (e1 , (Sum(t1,t2), Sum(t3,t4))::e2) -> unification (e1, (t1, t3)::(t2, t4)::e2) but
  | (_, (Sum(_,_), Prod(t3,t4))::_) -> raise (Echec_unif ("type somme non-unifiable avec "^(print_type (Prod(t3,t4)))))
  | (_, (Prod(t3,t4), Sum(_,_))::_) -> raise (Echec_unif ("type somme non-unifiable avec "^(print_type (Prod(t3,t4)))))
  | (_, (Sum(_, _), t3)::_) -> raise (Echec_unif ("type somme non-unifiable avec "^(print_type t3)))     
  | (_, (t3, Sum(_, _))::_) -> raise (Echec_unif ("type somme non-unifiable avec "^(print_type t3)))
    (* types produit des deux cotes : on decompose  *)
  | (e1 , (Prod(t1,t2), Prod(t3,t4))::e2) -> unification (e1, (t1, t3)::(t2, t4)::e2) but
  | (_, (Prod(_, _), t3)::_) -> raise (Echec_unif ("type produit non-unifiable avec "^(print_type t3)))     
  | (_, (t3, Prod(_, _))::_) -> raise (Echec_unif ("type produit non-unifiable avec "^(print_type t3)))     
                                       
(* enchaine generation d'equation et unification *)                                   
let inference (t : pterm) : string =
  let e : equa_zip = ([], genere_equa t (Var "but") []) in
  try (let res = unification e "but" in
       (print_term t)^" ***TYPABLE*** avec le type "^(print_type res))
  with Echec_unif bla -> (print_term t)^" ***PAS TYPABLE*** : "^bla

