open Ast


type address = int
type binding = address * pterm
type memory = binding list

let address_counter = ref 0

let new_address () =
  let a = !address_counter in
  incr address_counter;
  a

let cherch_mem a mem =
  try Some (List.assoc a mem) with Not_found -> None

let update_memory a v mem =
  (a, v) :: List.remove_assoc a mem

let extension_memoire v mem =
  let a = new_address () in
  let mem' = update_memory a v mem in
  (a, mem')


(* pretty printer de termes*)     
let rec print_term (t : pterm) : string =
  match t with
    Var x -> x
  | App (t1, t2) -> "(" ^ print_term t1 ^ " " ^ print_term t2 ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ print_term t ^ ")"
  | N n -> string_of_int n
  | Add (t1, t2) -> "(" ^ print_term t1 ^ " + " ^ print_term t2 ^ ")"
  | Mul (t1, t2) -> "(" ^ print_term t1 ^ " * " ^ print_term t2 ^ ")"
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
  | N n -> N n
  | Add (t1, t2) ->
      Add (alpha_convert t1 env, alpha_convert t2 env)
  | Mul (t1, t2) ->
      Mul (alpha_convert t1 env, alpha_convert t2 env)
  | Ifz (t1, t2, t3) ->
      Ifz (alpha_convert t1 env, alpha_convert t2 env, alpha_convert t3 env)
  | Succ t1 ->
      Succ (alpha_convert t1 env)
  | Pred t1 ->
      Pred (alpha_convert t1 env)
  | Couple (t1, t2) ->
      Couple (alpha_convert t1 env, alpha_convert t2 env)
  | ProdG t1 ->
      ProdG (alpha_convert t1 env)
  | ProdD t1 ->
      ProdD (alpha_convert t1 env)
  | SumG t1 ->
      SumG (alpha_convert t1 env)
  | SumD t1 ->
      SumD (alpha_convert t1 env)
  | MatchSum (t0, x1, t1, x2, t2) ->
      let x1' = nom_frais () in
      let x2' = nom_frais () in
      let env1 = (x1, x1') :: env in
      let env2 = (x2, x2') :: env in
      MatchSum (alpha_convert t0 env, x1', alpha_convert t1 env1, x2', alpha_convert t2 env2)
    | Let (x, t1, t2) ->
      let x' = nom_frais () in
      let env' = (x, x') :: env in
      Let (x', alpha_convert t1 env, alpha_convert t2 env')
  | Fix t1 ->
      Fix (alpha_convert t1 env)
  | Hd t1 ->
      Hd (alpha_convert t1 env)
  | Tl t1 ->
      Tl (alpha_convert t1 env)
  | IfEmpty (t1, t2, t3) ->
      IfEmpty (alpha_convert t1 env, alpha_convert t2 env, alpha_convert t3 env)
  | Liste lst ->
      let rec alpha_convert_list l =
        match l with
        | Empty -> Empty
        | Cons (head, tail) ->
            Cons (alpha_convert head env, alpha_convert_list tail)
      in
      Liste (alpha_convert_list lst)
  | Unit -> Unit
  | Address a -> Address a
  | Ref t1 -> Ref (alpha_convert t1 env)
  | DeRef t1 -> DeRef (alpha_convert t1 env)
  | Assign (t1, t2) ->
      Assign (alpha_convert t1 env, alpha_convert t2 env)   

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
   | N n -> N n
   | Add (t1, t2) ->
       Add (substituer nom remp t1, substituer nom remp t2)
   | Mul (t1, t2) ->
       Mul (substituer nom remp t1, substituer nom remp t2)
   | Ifz (t1, t2, t3) ->
       Ifz (substituer nom remp t1, substituer nom remp t2, substituer nom remp t3)
   | Succ t1 ->
       Succ (substituer nom remp t1)
   | Pred t1 ->
       Pred (substituer nom remp t1)
   | Couple (t1, t2) ->
       Couple (substituer nom remp t1, substituer nom remp t2)
   | ProdG t1 ->
       ProdG (substituer nom remp t1)
   | ProdD t1 ->
       ProdD (substituer nom remp t1)
   | SumG t1 ->
       SumG (substituer nom remp t1)
   | SumD t1 ->
       SumD (substituer nom remp t1)
   | MatchSum (t0, x1, t1, x2, t2) ->
       let t0' = substituer nom remp t0 in
       let t1' =
         if x1 = nom then t1
         else substituer nom remp t1
       in
       let t2' =
         if x2 = nom then t2
         else substituer nom remp t2
       in
       MatchSum (t0', x1, t1', x2, t2')
   | Let (x, t1, t2) ->
       let t1' = substituer nom remp t1 in
       let t2' =
         if x = nom then t2
         else substituer nom remp t2
       in
       Let (x, t1', t2')
   | Fix t1 ->
       Fix (substituer nom remp t1)
   | Hd t1 ->
       Hd (substituer nom remp t1)
   | Tl t1 ->
       Tl (substituer nom remp t1)
   | IfEmpty (t1, t2, t3) ->
       IfEmpty (substituer nom remp t1, substituer nom remp t2, substituer nom remp t3)
   | Liste lst ->
       let rec substituer_list l =
         match l with
         | Empty -> Empty
         | Cons (head, tail) ->
             Cons (substituer nom remp head, substituer_list tail)
       in
       Liste (substituer_list lst)
   | Unit -> Unit
   | Address a -> Address a
   | Ref t1 -> Ref (substituer nom remp t1)
   | DeRef t1 -> DeRef (substituer nom remp t1)
   | Assign (t1, t2) ->
       Assign (substituer nom remp t1, substituer nom remp t2)     
;;

let rec est_valeur (t : pterm) : bool =
  match t with
  | Var _ -> true
  | Abs _ -> true
  | N _ -> true
  | Unit -> true
  | Address _ -> true
  | Liste l -> liste_est_valeur l
  | Couple (v1, v2) -> est_valeur v1 && est_valeur v2
  | SumG v -> est_valeur v
  | SumD v -> est_valeur v
  | _ -> false
and liste_est_valeur (l : pterm liste) : bool =
  match l with
  | Empty -> true
  | Cons (head, tail) -> est_valeur head && liste_est_valeur tail
;;

(*reduction cbv left to right*)
(*adapter l'evaluation apres l'ajout des reference et la gestion des adresse -_- partie04*)
let rec etape_cbv (t : pterm) (mem : memory) : (pterm * memory) option =
  match t with

  | App (Abs (x, corps), v) when est_valeur v ->
      (* β-réduction *)
      Some (substituer x v corps, mem)

  | App (t1, t2) -> (
      match etape_cbv t1 mem with
      | Some (t1', mem') -> Some (App (t1', t2), mem')
      | None ->
          if est_valeur t1 then
            (match etape_cbv t2 mem with
             | Some (t2', mem') -> Some (App (t1, t2'), mem')
             | None -> None)
          else
            None
    )
  | Let(x, e1, e2) -> (
      match etape_cbv e1 mem with
      | Some (e1', mem') -> Some (Let (x, e1', e2), mem')    
      | None ->
          if est_valeur e1 then                   
            Some (substituer x e1 e2, mem)
          else None
    )
  | Fix (Abs(x, body)) ->
        Some (substituer x (Fix (Abs(x, body))) body, mem)
  | Fix phi -> (
      match etape_cbv phi mem with
      | Some (phi', mem') -> Some (Fix phi', mem')
      | None -> None
    )
  | Succ t1 -> (
      match etape_cbv t1 mem with
      | Some (t1', mem') -> Some (Succ t1', mem')
      | None ->
          match t1 with
          | N n -> Some (N (n+1), mem)
          | _ -> None
    )
    | Pred t1 -> (
        match etape_cbv t1 mem with
        | Some (t1', mem') -> Some (Pred t1', mem')
        | None ->
            match t1 with
            | N n when n > 0 -> Some (N (n-1), mem)
            | N 0 -> Some (N 0, mem)
            | _ -> None
        )
    | Add (t1, t2) -> (
        match etape_cbv t1 mem with
        | Some (t1', mem') -> Some (Add (t1', t2), mem')
        | None ->
            match t1 with
            | N n1 ->
                (match etape_cbv t2 mem with
                 | Some (t2', mem') -> Some (Add (t1, t2'), mem')
                 | None ->
                     match t2 with
                     | N n2 -> Some (N (n1 + n2), mem)
                     | _ -> None)
            | _ -> None
      )
    | Mul (t1, t2) -> (
        match etape_cbv t1 mem with
        | Some (t1', mem') -> Some (Mul (t1', t2), mem')
        | None ->
            match t1 with
            | N n1 ->
                (match etape_cbv t2 mem with
                 | Some (t2', mem') -> Some (Mul (t1, t2'), mem')
                 | None ->
                     match t2 with
                     | N n2 -> Some (N (n1 * n2), mem)
                     | _ -> None)
            | _ -> None
      )
    | Ifz (t1, t2, t3) -> (
        match etape_cbv t1 mem with
        | Some (t1', mem') -> Some (Ifz (t1', t2, t3), mem')
        | None ->
            match t1 with
            | N 0 -> Some (t2, mem)
            | N _ -> Some (t3, mem)
            | _ -> None
        )
    | IfEmpty (t1, t2, t3) -> (
        match etape_cbv t1 mem with
        | Some (t1', mem') -> Some (IfEmpty (t1', t2, t3), mem')
        | None ->
            match t1 with
            | Liste Empty -> Some (t2, mem)
            | Liste (Cons (_, _)) -> Some (t3, mem)
            | _ -> None
        )
  | Hd t1 -> (
      match etape_cbv t1 mem with
      | Some (t1', mem') -> Some (Hd t1', mem')
      | None ->
          match t1 with
          | Liste (Cons (head, _)) -> Some (head, mem)
          | _ -> None
    )
  | Tl t1 -> (
      match etape_cbv t1 mem with
      | Some (t1', mem') -> Some (Tl t1', mem')
      | None ->
          match t1 with
          | Liste (Cons (_, tail)) -> Some (Liste tail, mem)
          | _ -> None
    )
   | MatchSum (t0, x1, t1, x2, t2) -> (
      match etape_cbv t0 mem with
      | Some (t0', mem') -> Some (MatchSum (t0', x1, t1, x2, t2), mem')
      | None ->
          match t0 with
          | SumG v when est_valeur v -> Some (substituer x1 v t1, mem)
          | SumD v when est_valeur v -> Some (substituer x2 v t2, mem)
          | _ -> None
    )
    | Ref e -> (
        match etape_cbv e mem with
        | Some (e', mem') -> Some (Ref e', mem')
      | None ->
        if est_valeur e then
          let (a, mem') = extension_memoire e mem in
          Some (Address a, mem')
        else None
        )
    | DeRef e -> (
      match etape_cbv e mem with
       | Some (e', mem') -> Some (DeRef e', mem')
       | None ->
          match e with
            | Address a ->
              (match cherch_mem a mem with
                 | Some v -> Some (v, mem)
                 | None -> failwith "dereference: adresse inconnue")
                | _ -> None )

    | Assign (e1, e2) -> (
     match etape_cbv e1 mem with
      | Some (e1', mem') -> Some (Assign (e1', e2), mem')
      | None ->
        match e1 with
        | Address a ->
            (match  etape_cbv e2 mem with
             | Some (e2', mem') -> Some (Assign (e1, e2'), mem')
             | None ->
                 if est_valeur e2 then
                   let mem' = update_memory a e2 mem in
                   Some (Unit, mem')
                 else None)
        | _ -> None
  )
    | _ -> None

    
;;

(*normalisation*)
let rec evaluer_cbv (t : pterm) (mem : memory) : (pterm * memory) =
  match etape_cbv t mem with
  | Some (t', mem') -> evaluer_cbv t' mem'
  | None -> (t, mem)

let rec afficher_reductions (t : pterm) (mem : memory) =
  Printf.printf "%s   MEM=%s\n" (print_term t)
      (String.concat ";"
        (List.map (fun (a,v) -> Printf.sprintf "%d ↦ %s" a (print_term v)) mem));

  match etape_cbv t mem with
  | Some (t', mem') ->
      Printf.printf " => ";
      afficher_reductions t' mem'
  | None ->
      Printf.printf " => (forme normale)\n"
;;