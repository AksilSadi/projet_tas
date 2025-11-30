open Tas_lib.Ast
open Tas_lib.Typer
open Tas_lib.Evaluator

(* === ANSI color codes === *)
let green = "\027[32m"
let red   = "\027[31m"
let cyan  = "\027[36m"
let reset = "\027[0m"


(* ===================================================== *)
(* Helpers pour afficher les tests                       *)
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
  | exn ->
      Printf.printf "%s  ❌ Exception OCaml :%s %s\n" 
        red reset (Printexc.to_string exn)


let run_fail_test name expr =
  Printf.printf "%s[TEST NON TYPABLE]%s %s\n" cyan reset name;
  Printf.printf "  Expression : %s\n" (print_term expr);
  try
    let ty = inference_with_env expr [] in
    Printf.printf "%s  ❌ INCORRECT : expression typée alors qu’elle devrait échouer !%s\n"
      red reset;
    Printf.printf "     Type obtenu : %s%s\n\n"
      (print_type ty) reset
  with
  | Echec_unif msg ->
      Printf.printf "%s  ✔ Correct : Non typable →%s %s\n\n"
        green reset msg
  | _ ->
      Printf.printf "%s  ✔ Correct : Non typable (exception)%s\n\n"
        green reset


(* ====================================== *)
(*            TESTS TYPABLES              *)
(* ====================================== *)
let test_typables () =

  (* ref simple *)
  run_test "ref 3"
    (Ref (N 3));

  (* déréférencement *)
  run_test "! (ref 4)"
    (DeRef (Ref (N 4)));

  (* affectation simple *)
  run_test "let r = ref 0 in r := 5"
    (Let ("r", Ref (N 0),
      Assign (Var "r", N 5)));

  (* affectation + lecture *)
  run_test "let r = ref 2 in let _ = r := 3 in !r"
    (Let ("r", Ref (N 2),
      Let ("_", Assign (Var "r", N 3),
        DeRef (Var "r"))));

  (* interaction avec les listes *)
  run_test "let r = ref [1] in !r"
    (Let ("r", Ref (Liste (Cons (N 1, Empty))),
      DeRef (Var "r")));

  (* référence polymorphe NON expansif : let id = fun x -> x in ref id (interdit en ML mais le tien l'accepte faiblement) *)
  run_test "let id = fun x -> x in (id 3, id 4)"
    (Let ("id", Abs ("x", Var "x"),
      Couple (App (Var "id", N 3), App (Var "id", N 4))));

  (* région avec produit *)
  run_test "let r = ref (3,4) in !r"
    (Let ("r", Ref (Couple (N 3, N 4)),
      DeRef (Var "r")));

  (* assign sur un produit *)
  run_test "let r = ref (1,2) in r := (3,4)"
    (Let ("r", Ref (Couple (N 1, N 2)),
      Assign (Var "r", Couple (N 3, N 4))));

  ()


(* ====================================== *)
(*           TESTS NON TYPABLES           *)
(* ====================================== *)
let test_non_typables () =

  (* ! sur un entier *)
  run_fail_test "! 5"
    (DeRef (N 5));

  (* assign sur un entier *)
  run_fail_test "5 := 3"
    (Assign (N 5, N 3));

  (* assign avec type incohérent *)
  run_fail_test "let r = ref 1 in r := true"
    (Let ("r", Ref (N 1),
      Assign (Var "r", Abs ("x", Var "x"))));  (* fonction ≠ Nat *)

  (* ! sur une fonction *)
  run_fail_test "! (fun x -> x)"
    (DeRef (Abs ("x", Var "x")));

  (* ref d’une expression mal typée *)
  run_fail_test "ref (1 + (fun x -> x))"
    (Ref (Add (N 1, Abs ("x", Var "x"))));

  (* cas du polymorphisme faible interdit *)
  run_fail_test "let r = ref [] in (r := [3]; hd !r)"
    (Let ("r", Ref (Liste Empty),
      Let ("_", Assign (Var "r", Liste (Cons (N 3, Empty))),
        Hd (DeRef (Var "r")) )));

  ()

(* ====================== *)
(* MAIN                   *)
(* ====================== *)

let () =
  Printf.printf "\n=== TESTS TRAITS IMPÉRATIFS ===\n";
  Printf.printf "=== TYPABLES ===\n";
  test_typables ();

  Printf.printf "\n=== NON TYPABLES ===\n";
  test_non_typables ();
