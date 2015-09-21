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
showAut (AutAtomicFin phi)      = "Aᶠ{" ++ phi ++ "}"
showAut (AutAtomicNonfin phi)   = "Aⁿ{" ++ phi ++ "}"
showAut (AutUnionFin a1 a2)     = "(" ++ (showAut a1) ++ ") ∪ᶠ (" ++ (showAut a2) ++ ")"
showAut (AutUnionNonfin a1 a2)  = "(" ++ (showAut a1) ++ ") ∪ⁿ (" ++ (showAut a2) ++ ")"
showAut (AutIsectFin a1 a2)     = "(" ++ (showAut a1) ++ ") ∩ᶠ (" ++ (showAut a2) ++ ")"
showAut (AutIsectNonfin a1 a2)  = "(" ++ (showAut a1) ++ ") ∩ⁿ (" ++ (showAut a2) ++ ")"
showAut (AutComplFin a)       = "Cᶠ(" ++ (showAut a) ++ ")"
showAut (AutComplNonfin a)    = "Cⁿ(" ++ (showAut a) ++ ")"
showAut (AutProjFin var a)    = "πᶠ[" ++ [var] ++ "](" ++ (showAut a) ++ ")"
showAut (AutProjNonfin var a) = "πⁿ[" ++ [var] ++ "](" ++ (showAut a) ++ ")"

-- instantiance of the data type as class Show
instance Show Aut where
  show = showAut


-- translates a formula into an automaton represented using final states
toAutomatonFin :: Logic.Formula -> Aut
toAutomatonFin (Logic.FormulaAtomic phi) = AutAtomicFin phi
toAutomatonFin (Logic.Disj f1 f2)        = (toAutomatonFin f1) `AutUnionFin` (toAutomatonFin f2)
toAutomatonFin (Logic.Conj f1 f2)        = (toAutomatonFin f1) `AutIsectFin` (toAutomatonFin f2)
toAutomatonFin (Logic.Neg f)             = AutComplFin $ toAutomatonNonfin f
toAutomatonFin (Logic.Exists var f)      = AutProjFin var $ toAutomatonFin f
toAutomatonFin f@(Logic.ForAll _ _)      = toAutomatonFin $ Logic.removeForAll f

-- translates a formula into an automaton represented using _non_final states
toAutomatonNonfin :: Logic.Formula -> Aut
toAutomatonNonfin (Logic.FormulaAtomic phi) = AutAtomicNonfin phi
toAutomatonNonfin (Logic.Disj f1 f2)        = (toAutomatonNonfin f1) `AutUnionNonfin` (toAutomatonNonfin f2)
toAutomatonNonfin (Logic.Conj f1 f2)        = (toAutomatonNonfin f1) `AutIsectNonfin` (toAutomatonNonfin f2)
toAutomatonNonfin (Logic.Neg f)             = AutComplNonfin $ toAutomatonFin f
toAutomatonNonfin (Logic.Exists var f)      = AutProjNonfin var $ toAutomatonNonfin f
toAutomatonNonfin f@(Logic.ForAll _ _)      = toAutomatonNonfin $ Logic.removeForAll f

-- top-level function that translates a formula into an automaton
toAutomaton :: Logic.Formula -> Aut
toAutomaton = toAutomatonFin


type State = String

data StateTerm =
    StateSet [State]
  | StateTermUnionFin StateTerm StateTerm
  | StateTermUnionNonfin StateTerm StateTerm
  | StateTermIsectFin StateTerm StateTerm
  | StateTermIsectNonfin StateTerm StateTerm
  | StateTermUpClosed StateTerm

-- print out a state term in a human-readable format
showStateTerm :: StateTerm -> String
showStateTerm (StateSet xs)                = show xs
showStateTerm (StateTermUnionFin t1 t2)    = "(" ++ (showStateTerm t1) ++ " ᵤ×ᶠ " ++ (showStateTerm t2) ++ ")"
showStateTerm (StateTermUnionNonfin t1 t2) = "(" ++ (showStateTerm t1) ++ " ᵤ×ⁿ " ++ (showStateTerm t2) ++ ")"
showStateTerm (StateTermIsectFin t1 t2)    = "(" ++ (showStateTerm t1) ++ " ᵢ×ᶠ " ++ (showStateTerm t2) ++ ")"
showStateTerm (StateTermIsectNonfin t1 t2) = "(" ++ (showStateTerm t1) ++ " ᵢ×ⁿ " ++ (showStateTerm t2) ++ ")"
showStateTerm (StateTermUpClosed t)        = "↑{" ++ showStateTerm t ++ "}"

-- instantiance of the data type as class Show
instance Show StateTerm where
  show = showStateTerm


-- initial states
initial :: Aut -> StateTerm
initial (AutAtomicFin phi)
  | phi == "X ⊆ Y"    = StateSet ["q1"]
  | phi == "X ⊇ Y"    = StateSet ["q2"]
  | phi == "Z = σ(Y)" = StateSet ["q3"]
  | otherwise         = error "Unknown atomic predicate"
initial (AutAtomicNonfin phi)
  | phi == "X ⊆ Y"    = StateSet ["q1"]
  | phi == "X ⊇ Y"    = StateSet ["q2"]
  | phi == "Z = σ(Y)" = StateSet ["q3"]
  | otherwise         = error "Unknown atomic predicate"
initial (AutUnionFin a1 a2)    = (initial a1) `StateTermUnionFin` (initial a2)
initial (AutUnionNonfin a1 a2) = (initial a1) `StateTermUnionNonfin` (initial a2)
initial (AutIsectFin a1 a2)    = (initial a1) `StateTermIsectFin` (initial a2)
initial (AutIsectNonfin a1 a2) = (initial a1) `StateTermIsectNonfin` (initial a2)
initial (AutComplFin a)        = StateTermUpClosed $ initial a
initial (AutComplNonfin a)     = StateTermUpClosed $ initial a
initial (AutProjFin var a)     = initial a
initial (AutProjNonfin var a)  = initial a


-- the automaton for the example formula
exampleAutomaton :: Aut
exampleAutomaton = toAutomaton Logic.exampleFormula

