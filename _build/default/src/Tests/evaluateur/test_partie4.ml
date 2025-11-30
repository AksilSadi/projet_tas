open Tas_lib.Ast
open Tas_lib.Evaluator

(* Fonction utilitaire : affichage d'une mémoire *)
let string_of_memory (mem : memory) : string =
  "[" ^
  (String.concat "; "
     (List.map (fun (a,v) ->
        Printf.sprintf "%d ↦ %s" a (print_term v)
     ) mem)
  ) ^ "]"


(* Affiche le terme, sa forme normale, et la mémoire finale *)
let show_ref_eval t =
  Printf.printf "Terme     : %s\n" (print_term t);
  let (nf, memf) = evaluer_cbv t [] in
  Printf.printf "NF        : %s\n" (print_term nf);
  Printf.printf "Mémoire   : %s\n\n" (string_of_memory memf)
;;


let line () =
  print_endline "-------------------------------------------"
;;


(* ====================================================== *)
(*  TEST 1 — simple ref                                    *)
(* ====================================================== *)

let test_ref_simple () =
  print_endline "TEST 1 : ref 5";
  let t = Ref (N 5) in
  show_ref_eval t;
  line ()
;;


(* ====================================================== *)
(*  TEST 2 — déréférencement                              *)
(* ====================================================== *)

let test_deref () =
  print_endline "TEST 2 : let r = ref 10 in !r";

  let t =
    Let ("r",
         Ref (N 10),
         DeRef (Var "r"))
  in

  show_ref_eval t;
  line ()
;;


(* ====================================================== *)
(*  TEST 3 — affectation simple                           *)
(* ====================================================== *)

let test_assign () =
  print_endline "TEST 3 : let r = ref 2 in (r := 7 ; !r)";

  let t =
    Let ("r",
         Ref (N 2),
         Let ("_",
              Assign (Var "r", N 7),
              DeRef (Var "r")))
  in

  show_ref_eval t;
  line ()
;;


(* ====================================================== *)
(*  TEST 4 — aliasing                                     *)
(* ====================================================== *)

let test_aliasing () =
  print_endline "TEST 4 : aliasing :";
  print_endline "      let r = ref 3 in let s = r in (s := 10 ; !r)";

  let t =
    Let ("r", Ref (N 3),
      Let ("s", Var "r",
        Let ("_", Assign (Var "s", N 10),
            DeRef (Var "r"))))
  in

  show_ref_eval t;
  line ()
;;


(* ====================================================== *)
(*  TEST 5 — référence de liste                           *)
(* ====================================================== *)

let test_ref_list () =
  print_endline "TEST 5 : let r = ref [1,2] in hd !r";

  let l = Liste (Cons (N 1, Cons (N 2, Empty))) in

  let t =
    Let ("r", Ref l,
         Hd (DeRef (Var "r")))
  in

  show_ref_eval t;
  line ()
;;


(* ====================================================== *)
(*  TEST 6 — modification de liste dans une ref           *)
(* ====================================================== *)

let test_mutate_list () =
  print_endline "TEST 6 : let r = ref [] in (r := [3] ; hd !r)";

  let t =
    Let ("r", Ref (Liste Empty),
        Let ("_", Assign (Var "r",
                          Liste (Cons (N 3, Empty))),
             Hd (DeRef (Var "r"))))
  in

  show_ref_eval t;
  line ()
;;


(* ====================================================== *)
(*  TEST 7 — références imbriquées                        *)
(* ====================================================== *)

let test_nested_refs () =
  print_endline "TEST 7 : ref (ref 4)  puis !!";

  let t =
    Let ("r",
         Ref (Ref (N 4)),
         DeRef (DeRef (Var "r")))
  in

  show_ref_eval t;
  line ()
;;


(* ====================================================== *)
(*  TEST 8 — plusieurs adresses en jeu                    *)
(* ====================================================== *)

let test_multi_refs () =
  print_endline "TEST 8 : création de plusieurs refs";

  let t =
    Let ("a", Ref (N 1),
      Let ("b", Ref (N 2),
        Let ("c", Ref (N 3),
            Add (DeRef (Var "a"),
                 Add (DeRef (Var "b"),
                      DeRef (Var "c"))))))
  in

  show_ref_eval t;
  line ()
;;



(* ====================================================== *)
(* LANCEMENT DES TESTS                                    *)
(* ====================================================== *)

let () =
  print_endline "\n========= TESTS PARTIE 4 : REFERENCES =========\n";

  test_ref_simple ();
  test_deref ();
  test_assign ();
  test_aliasing ();
  test_ref_list ();
  test_mutate_list ();
  test_nested_refs ();
  test_multi_refs ();

  print_endline "============= FIN DES TESTS PARTIE 4 =============\n"
;;
