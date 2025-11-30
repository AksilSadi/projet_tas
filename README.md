# Projet-TAS
Projet réalisé dans le cadre du module TAS (Types & Sémantique) par Aksil Sadi etudiant en Master2 STL sorbonne universite.
L’objectif : implémenter un évaluateur et un typeur pour un λ-calcul progressivement enrichi (entiers, listes, fix, let-polymorphisme, références, etc.).

# 📁 Structure du projet

```bash
src/
│
├── ast.ml              (* Définition de l’AST complet *)
├── utils.ml            
├── evaluator.ml        (* Évaluateur CBV gauche→droite *)
├── typer.ml            (* Typeur + unification *)
├── main.ml             (* Point d’entrée : exécution d’un terme *)
│
├── Tests/
│   ├── evaluateur/
│   │      ├── test_partie2.ml   (* λ-calcul simple : α, β, substitution *)
│   │      ├── test_partie3.ml   (* entiers, listes, let, fix, sommes *)
│   │      ├── test_partie4.ml   (* références, mémoire, unit *)
│   │      └── dune
│   │
│   └── typage/
│          ├── test_partie3.ml   (* tests de typage des entiers/listes *)
│          ├── test_partie4.ml   (* tests du typage des références *)
│          └── dune
│
├── dune                  (* Build des modules du projet *)
└── dune-project
```

# Fonctionnalités réalisées

## ⭐ Partie 2 — λ-calcul

-Syntaxe : Var, Abs, App
-Pretty-printer
-α-conversion correcte (évite la capture)
-Substitution
-Évaluation Left-to-Right Call-by-Value
-Normalisation
-Tests complets automatisés

## ⭐ Partie 3 - Entiers, Listes, Let, Fix, Sommes, Produits

-Entiers : N, Add, Mul, Pred, Succ, Ifz
-Listes : Liste, Hd, Tl, IfEmpty
-Fixpoint : Fix
-Let natif : Let
-Types produits (ProdG, ProdD, Couple)
-Types sommes (SumG, SumD, MatchSum)

Mise à jour complète :

-α-conversion
-substitution
-générateur d’équations
-unification
-évaluateur
-Tests de typage + tests d’évaluation

## ⭐ Partie 4 - Traits impératifs

-Valeur Unit
-Références : Ref e
-Deref : !e
-Assignation : e1 := e2
-Gestion d’une mémoire mutable :
-allocation (memory_extend)
-lookup (mem_lookup)
-update (mem_update)
-aliasing correct
-Évaluation (terme, mémoire) → (terme, mémoire)
-Tests complets affichant l’état mémoire final

## ⭐ Polymorphisme faible

Implémenté à 90% :
-variables expansives → schéma Weak
-non-expansives → schéma Strong

Il manque seulement un petit ajustement dans la ré-instanciation / la généralisation.

# Fonctionnalités non réalisées
-objets (classes/méthodes)
-exceptions
-sous-typage / rangées de polymorphisme

# ▶️ Compilation & Exécution

Tu peux compiler tout le projet avec :

```bash
dune build
```

Lancer le binaire principal :

```bash
dune exec tas
```

# 🧪 Lancer les tests

Les tests sont regroupés par partie.

## ▶️ Tester l’évaluateur : (PARTIE 2,3,4)

```bash
dune exec Tests/evaluateur/test_partie2.exe
dune exec Tests/evaluateur/test_partie3.exe
dune exec Tests/evaluateur/test_partie4.exe

```
## ▶️ Tester typeur : (PARTIE 3,4)

```bash
dune exec Tests/typage/test_partie3.exe
dune exec Tests/typage/test_partie4.exe

```