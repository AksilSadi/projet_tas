open Tas_lib.Ast
open Tas_lib.Evaluator


let show_nf t =
  let nf = evaluer_cbv t in
  Printf.printf "Resultat   : %s\n\n" (print_term nf)

let line () =
  print_endline "-----------------------------------------------"
;;

(* ============================================================ *)
(*  TEST 1 : opérations sur les entiers                         *)
(* ============================================================ *)

let test_entiers () =
  print_endline "TEST 1 : opérations arithmétiques";
  let t = Add (N 3, Add (N 4, N 5)) in
  Printf.printf "Term : %s\n" (print_term t);
  show_nf t;
  line ()
;;

(* ============================================================ *)
(*  TEST 2 : succ / pred / ifz                                 *)
(* ============================================================ *)

let test_ifzero () =
  print_endline "TEST 2 : succ/pred + ifz";
  let t =
    Ifz (Pred (N 1),
         N 42,
         N 99)
  in
  Printf.printf "Term : %s\n" (print_term t);
  show_nf t;
  line ()
;;

(* ============================================================ *)
(*  TEST 3 : listes natives + hd/tl + ifempty                   *)
(* ============================================================ *)

let test_listes () =
  print_endline "TEST 3 : listes + hd/tl + ifempty";

  let l = Liste (Cons (N 10, Cons (N 20, Empty))) in
  let t1 = Hd l in
  let t2 = Tl l in

  Printf.printf "Term hd : %s\n" (print_term t1);
  show_nf t1;

  Printf.printf "Term tl : %s\n" (print_term t2);
  show_nf t2;

  let t3 = IfEmpty (l, N 0, N 999) in
  Printf.printf "Term ifempty : %s\n" (print_term t3);
  show_nf t3;

  line ()
;;

(* ============================================================ *)
(*  TEST 4 : let-binding                                        *)
(* ============================================================ *)

let test_let () =
  print_endline "TEST 4 : let x = 5+3 in x+2";

  let t =
    Let ("x",
         Add (N 5, N 3),
         Add (Var "x", N 2))
  in
  Printf.printf "Term : %s\n" (print_term t);
  show_nf t;
  line ()
;;

(* ============================================================ *)
(*  TEST 5 : fix — fonction récursive factorielle               *)
(* ============================================================ *)

let test_fix_fact () =
  print_endline "TEST 5 : fix (lambda fact. lambda n. ifz n then 1 else n * fact (n-1))";

  let fact_term =
    Fix (
      Abs ("fact",
           Abs ("n",
                Ifz (Var "n",
                     N 1,
                     Mul (Var "n",
                          App (Var "fact",
                               Pred (Var "n")))
                    )
               )
          )
    )
  in

  (* Appliquer fact(4) *)
  let test = App (fact_term, N 4) in

  Printf.printf "Term : %s\n" (print_term test);
  show_nf test;
  line ()
;;

(* ============================================================ *)
(*  TEST 6 : match sum                                          *)
(* ============================================================ *)

let test_somme () =
  print_endline "TEST 6 : match sur les sommes";

  (* inl 7 *)
  let t =
    MatchSum (SumG (N 7),
              "x", Add (Var "x", N 1),
              "y", Add (Var "y", N 100))
  in

  Printf.printf "Term : %s\n" (print_term t);
  show_nf t;

  (* inr 3 *)
  let t2 =
    MatchSum (SumD (N 3),
              "x", Add (Var "x", N 1),
              "y", Add (Var "y", N 100))
  in

  Printf.printf "Term : %s\n" (print_term t2);
  show_nf t2;

  line ()
;;

(* ============================================================ *)
(*  LANCEMENT DE TOUS LES TESTS                                 *)
(* ============================================================ *)

let () =
  print_endline "\n==== TESTS ÉVALUATEUR — PARTIE 3 ====\n";

  test_entiers ();
  test_ifzero ();
  test_listes ();
  test_let ();
  test_fix_fact ();
  test_somme ();

  print_endline "==== FIN DES TESTS ===="
;;
