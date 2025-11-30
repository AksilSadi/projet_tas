open Tas_lib.Ast
open Tas_lib.Evaluator



let show_norm term =
  Printf.printf "Term : %s\n" (print_term term);
  let nf = evaluer_cbv term in
  Printf.printf "Resultat   : %s\n\n" (print_term nf)



(*  TEST 1 : β-réduction simple *)
let test_beta_simple () =
  let t =
    App (Abs ("x", Var "x"), Var "y")
  in
  print_endline "TEST 1 : beta-reduction simple ( (λx.x) y )";
  show_norm t


(*  TEST 2 : réduction imbriquée  *)
let test_beta_nested () =
  let t =
    App (Abs ("x", App (Var "x", Var "x")),
         Abs ("z", Var "z"))
  in
  print_endline "TEST 2 : reduction imbriquee ( (λx.x x) (λz.z) )";
  show_norm t


(*  TEST 3 : éviter la capture (α-conversion)  *)
let test_alpha_conversion () =
  let t =
    App (Abs ("x", Abs ("y", Var "x")),
         Var "y")
  in
  print_endline "TEST 3 : alpha-conversion sécurisée";
  print_endline ("Avant : " ^ print_term t);
  let t' = alpha_convert t [] in
  print_endline ("Après : " ^ print_term t');
  print_newline ()


(* TEST 4 : application nécessitant plusieurs étapes  *)
let test_multi_reduction () =
  let t =
    App (
      App (Abs ("x", Abs ("y", Var "x")), Var "a"),
      Var "b"
    )
  in
  print_endline "TEST 4 : reduction en plusieurs etapes";
  show_norm t


(*  TEST 5 : terme bloqué (forme normale)  *)
let test_stuck () =
  let t = App (Var "x", Var "y") in
  print_endline "TEST 5 : terme bloque (pas de reduction possible)";
  show_norm t


(*  EXECUTION DE TOUS LES TESTS  *)
let () =
  print_endline "===== TESTS PARTIE 2 - EVALUATEUR =====\n";

  test_beta_simple ();
  test_beta_nested ();
  test_alpha_conversion ();
  test_multi_reduction ();
  test_stuck ();

  print_endline "===== FIN DES TESTS ====="
