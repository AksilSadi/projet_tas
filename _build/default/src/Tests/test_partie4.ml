open Tas_lib.Ast
open Tas_lib.Typer

(* === ANSI color codes === *)
let green = "\027[32m"
let red   = "\027[31m"
let cyan  = "\027[36m"
let reset = "\027[0m"


(* ===================================================== *)
(* Fonction pour tests typables                          *)
(* ===================================================== *)
let run_test name expr =
  Printf.printf "%s[TEST]%s %s\n" cyan reset name;
  try
    let ty = inference_with_env expr [] in
    Printf.printf "  Expression : %s\n" (print_term expr);
    Printf.printf "  Type       : %s%s%s\n\n"
      green (print_type ty) reset
  with
  | Echec_unif msg ->
      Printf.printf "%s  ❌ Erreur d’unification :%s %s\n\n"
        red reset msg
  | _ ->
      Printf.printf "%s  ❌ Erreur inconnue%s\n\n" red reset


(* ===================================================== *)
(* Fonction pour tests NON typables                      *)
(* ===================================================== *)
let run_fail_test name expr =
  Printf.printf "%s[TEST NON TYPABLE]%s %s\n" cyan reset name;
  Printf.printf "  Expression : %s\n" (print_term expr);
  try
    let ty = inference_with_env expr [] in
    (* Si ça passe → erreur *)
    Printf.printf "%s  ❌ Mauvais : cette expression est typée alors qu’elle devrait échouer.\n"
      red;
    Printf.printf "     Type obtenu : %s%s\n\n"
      (print_type ty) reset
  with
  | Echec_unif msg ->
      Printf.printf "%s  ❌ Non typable :%s %s\n\n"
        red reset msg
  | _ ->
      Printf.printf "%s  ❌ Non typable (erreur inconnue)%s\n\n"
        red reset


(* ================== *)
(* TESTS ENTIER / NAT *)
(* ================== *)
let test_nat () =
  run_test "Entier simple"
    (N 3);

  run_test "Addition"
    (Add (N 1, N 2));

  run_test "Expression arithmétique"
    (Add (N 5, Succ (Pred (N 8))))


(* ==================== *)
(* TESTS LISTES PARTIE4 *)
(* ==================== *)
let test_listes () =
  run_test "Liste vide"
    (Liste Empty);

  run_test "Liste [1]"
    (Liste (Cons (N 1, Empty)));

  run_test "Liste [1;2;3]"
    (Liste (Cons (N 1, Cons (N 2, Cons (N 3, Empty)))));

  run_test "hd [1;2;3]"
    (Hd (Liste (Cons (N 1, Cons (N 2, Cons (N 3, Empty))))));

  run_test "tl [1;2;3]"
    (Tl (Liste (Cons (N 1, Cons (N 2, Cons (N 3, Empty))))))


(* ====================== *)
(* TESTS POLYMORPHISME    *)
(* ====================== *)
let test_poly () =
  let id_fun = Abs ("x", Var "x") in

  run_test "let id = fun x -> x in id 3"
    (Let ("id", id_fun,
      App (Var "id", N 3)));

  run_test "let id = fun x -> x in id [1]"
    (Let ("id", id_fun,
      App (Var "id", Liste (Cons (N 1, Empty)))));

  run_test "let id = fun x -> x in (id 3, id [1])"
    (Let ("id", id_fun,
      Couple (
        App (Var "id", N 3),
        App (Var "id", Liste (Cons (N 1, Empty)))
      )))


(* =========================== *)
(* TESTS NON TYPABLES (ERREURS) *)
(* =========================== *)

let test_non_typable () =

  run_fail_test "Appel d’un entier comme fonction"
    (App (N 3, N 5));

  run_fail_test "Addition avec une fonction"
    (Add (N 1, Abs ("x", Var "x")));

  run_fail_test "hd appliqué à un entier"
    (Hd (N 10));

  run_fail_test "tl appliqué à une fonction"
    (Tl (Abs ("x", Var "x")));

  run_fail_test "Liste hétérogène"
    (Liste (Cons (N 1, Cons (Abs ("x", Var "x"), Empty))));

  run_fail_test "Ifz avec condition non-Nat"
    (Ifz (Abs ("x", Var "x"), N 1, N 2));

  run_fail_test "IfEmpty sur un entier"
    (IfEmpty (N 3, N 1, N 2));

  run_fail_test "Projection π₁ sur un entier"
    (ProdG (N 42));

  run_fail_test "match somme sur un entier"
    (MatchSum (N 3, "x", Var "x", "y", Var "y"));

  run_fail_test "Fix sans abstraction"
    (Fix (N 3));

  run_fail_test "Polymorphisme cassé"
    (Let ("id", Abs ("x", Var "x"),
      Add (
        App (Var "id", N 3),
        App (Var "id", Abs ("y", Var "y"))
      )))


(* ====================== *)
(* MAIN                   *)
(* ====================== *)

let () =
  Printf.printf "\n=== TESTS NAT ===\n";
  test_nat ();

  Printf.printf "\n=== TESTS LISTES ===\n";
  test_listes ();

  Printf.printf "\n=== TESTS POLYMORPHISME ===\n";
  test_poly ();

  Printf.printf "\n=== TESTS NON TYPABLES ===\n";
  test_non_typable ();
