open Ast

(* pretty printer de termes*)     
let rec print_term (t : pterm) : string =
  match t with
    Var x -> x
  | App (t1, t2) -> "(" ^ print_term t1 ^ " " ^ print_term t2 ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ print_term t ^ ")"
  | N n -> string_of_int n
  | Add (t1, t2) -> "(" ^ print_term t1 ^ " + " ^ print_term t2 ^ ")"
  | Ifz (t1, t2, t3) ->
      "(ifz " ^ print_term t1 ^ " then " ^ print_term t2 ^ " else " ^ print_term t3 ^ ")"
  | Succ t1 -> "(succ " ^ print_term t1 ^ ")"
  | Pred t1 -> "(pred " ^ print_term t1 ^ ")"
  | Couple (t1, t2) -> "(" ^ print_term t1 ^ ", " ^ print_term t2 ^ ")"
  | ProdG t1 -> "(π₁ " ^ print_term t1 ^ ")"
  | ProdD t1 -> "(π₂ " ^ print_term t1 ^ ")"
  | SumG t1 -> "(inl " ^ print_term t1 ^ ")"
  | SumD t1 -> "(inr " ^ print_term t1 ^ ")"
  | MatchSum (t0, x1, t1, x2, t2) ->
      "(match " ^ print_term t0 ^ " with inl "
      ^ x1 ^ " -> " ^ print_term t1 ^ " | inr "
      ^ x2 ^ " -> " ^ print_term t2 ^ ")"
  | Let (x, t1, t2) ->
      "(let " ^ x ^ " = " ^ print_term t1 ^ " in " ^ print_term t2 ^ ")"
  | Fix t1 -> "(fix " ^ print_term t1 ^ ")"
  | Hd t1 -> "(hd " ^ print_term t1 ^ ")"
  | Tl t1 -> "(tl " ^ print_term t1 ^ ")"
  | IfEmpty (t1, t2, t3) ->
      "(ifempty " ^ print_term t1 ^ " then " ^ print_term t2 ^ " else " ^ print_term t3 ^ ")"
  | Liste lst -> string_of_pterm_list lst
  | Unit -> "()"
  | Ref t1 -> "(ref " ^ print_term t1 ^ ")"
  | DeRef t1 -> "(!" ^ print_term t1 ^ ")"
  | Assign (t1, t2) -> "(" ^ print_term t1 ^ " := " ^ print_term t2 ^ ")"
  | Address addr -> "(addr " ^ string_of_int addr ^ ")"

and string_of_pterm_list (lst : pterm liste) : string =
  let rec aux_list l =
    match l with
    | Empty -> ""
    | Cons (head, Empty) -> print_term head
    | Cons (head, tail) -> print_term head ^ "," ^ aux_list tail
  in
  "[" ^ aux_list lst ^ "]"

(* génération de noms frais pour l'alpha-conversion *)
let compteur_renom = ref 0

let nom_frais () =
  incr compteur_renom;
  "Z" ^ string_of_int !compteur_renom

(* alpha_convert : pterm -> (string*string) list -> pterm *)
let rec alpha_convert (t : pterm) (env : (string * string) list) : pterm =
  match t with

  | Var x ->
      (* si x est dans env, on remplace *)
      (match List.assoc_opt x env with
       | Some y -> Var y
       | None -> Var x)

  | App (t1, t2) ->
      App (alpha_convert t1 env, alpha_convert t2 env)

  | Abs (x, corps) ->
      let x' = nom_frais () in
      let env' = (x, x') :: env in
      Abs (x', alpha_convert corps env')
  | _-> raise (Failure "alpha_convert: not implemented for this term")
;;

let rec substituer (nom : string) (remp : pterm) (t : pterm) : pterm =
  match t with

  | Var x ->
      if x = nom then remp else t

  | App (t1, t2) ->
      App (substituer nom remp t1, substituer nom remp t2)

  | Abs (x, corps) ->
      if x = nom then
        t  (* la variable liée bloque la substitution *)
      else
        let nouv = nom_frais () in
        let corps_alpha = substituer x (Var nouv) corps in
        Abs (nouv, substituer nom remp corps_alpha)
   | _-> raise (Failure "substituer: not implemented for this term")
;;

let est_valeur (t : pterm) : bool =
  match t with
  | Abs _ -> true
  | _ -> false
;;

(*reduction cbv left to right*)

let rec etape_cbv (t : pterm) : pterm option =
  match t with

  | App (Abs (x, corps), v) when est_valeur v ->
      (* β-réduction *)
      Some (substituer x v corps)

  | App (t1, t2) -> (
      match etape_cbv t1 with
      | Some t1' -> Some (App (t1', t2))
      | None ->
          if est_valeur t1 then
            (match etape_cbv t2 with
             | Some t2' -> Some (App (t1, t2'))
             | None -> None)
          else
            None
    )

  | _ -> None
;;

(*normalisation*)
let rec normaliser (t : pterm) : pterm =
  match etape_cbv t with
  | Some t' -> normaliser t'
  | None -> t
;;

let rec afficher_reductions (t : pterm) =
  Printf.printf "%s\n" (print_term t);
  match etape_cbv t with
  | Some t' ->
      Printf.printf " => ";
      afficher_reductions t'
  | None ->
      Printf.printf " => (forme normale)\n"
;;