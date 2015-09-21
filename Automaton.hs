module Automaton where

import qualified Logic
import Logic (Var)

-- hierarchical automaton
data Aut =
    AutAtomicFin String
  | AutAtomicNonfin String
  | AutUnionFin Aut Aut
  | AutUnionNonfin Aut Aut
  | AutIsectFin Aut Aut
  | AutIsectNonfin Aut Aut
  | AutComplFin Aut
  | AutComplNonfin Aut
  | AutProjFin Var Aut
  | AutProjNonfin Var Aut

-- prints the automaton in a human-readable format
showAut :: Aut -> String
showAut (AutAtomicFin phi)         = "Aᶠ{" ++ phi ++ "}"
showAut (AutAtomicNonfin phi)      = "Aⁿ{" ++ phi ++ "}"
showAut (AutUnionFin aut1 aut2)    = "(" ++ (showAut aut1) ++ ") ∪ᶠ (" ++ (showAut aut2) ++ ")"
showAut (AutUnionNonfin aut1 aut2) = "(" ++ (showAut aut1) ++ ") ∪ⁿ (" ++ (showAut aut2) ++ ")"
showAut (AutIsectFin aut1 aut2)    = "(" ++ (showAut aut1) ++ ") ∩ᶠ (" ++ (showAut aut2) ++ ")"
showAut (AutIsectNonfin aut1 aut2) = "(" ++ (showAut aut1) ++ ") ∩ⁿ (" ++ (showAut aut2) ++ ")"
showAut (AutComplFin aut)          = "Cᶠ(" ++ (showAut aut) ++ ")"
showAut (AutComplNonfin aut)       = "Cⁿ(" ++ (showAut aut) ++ ")"
showAut (AutProjFin var aut)       = "πᶠ[" ++ [var] ++ "](" ++ (showAut aut) ++ ")"
showAut (AutProjNonfin var aut)    = "πⁿ[" ++ [var] ++ "](" ++ (showAut aut) ++ ")"

-- instantiance of the data type as class Show
instance Show Aut where
  show = showAut

-- translates a formula into an automaton represented using final states
toAutomatonFin :: Logic.Formula -> Aut
toAutomatonFin (Logic.FormulaAtomic phi) = AutAtomicFin phi
toAutomatonFin (Logic.Disj f1 f2)        = AutUnionFin (toAutomatonFin f1) (toAutomatonFin f2)
toAutomatonFin (Logic.Conj f1 f2)        = AutIsectFin (toAutomatonFin f1) (toAutomatonFin f2)
toAutomatonFin (Logic.Neg f)             = AutComplFin $ toAutomatonNonfin f
toAutomatonFin (Logic.Exists var f)      = AutProjFin var $ toAutomatonFin f
toAutomatonFin f@(Logic.ForAll _ _)      = toAutomatonFin $ Logic.removeForAll f

-- translates a formula into an automaton represented using _non_final states
toAutomatonNonfin :: Logic.Formula -> Aut
toAutomatonNonfin (Logic.FormulaAtomic phi) = AutAtomicNonfin phi
toAutomatonNonfin (Logic.Disj f1 f2)        = AutUnionNonfin (toAutomatonNonfin f1) (toAutomatonNonfin f2)
toAutomatonNonfin (Logic.Conj f1 f2)        = AutIsectNonfin (toAutomatonNonfin f1) (toAutomatonNonfin f2)
toAutomatonNonfin (Logic.Neg f)             = AutComplNonfin $ toAutomatonFin f
toAutomatonNonfin (Logic.Exists var f)      = AutProjNonfin var $ toAutomatonNonfin f
toAutomatonNonfin f@(Logic.ForAll _ _)      = toAutomatonNonfin $ Logic.removeForAll f

-- top-level function that translates a formula into an automaton
toAutomaton :: Logic.Formula -> Aut
toAutomaton = toAutomatonFin




-- the automaton for the example formula
exampleAutomaton :: Aut
exampleAutomaton = toAutomaton Logic.exampleFormula

