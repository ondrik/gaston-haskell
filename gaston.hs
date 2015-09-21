--
-- Gaston: A prototype implementation of a new decision procedure for WS1S
--





--
--                         Useful UTF-8 Characters
--                         =======================
-- Logical symbols:
-- ¬ ∃ ∀ ⋁ ⋀ ∧ ∨
--
-- Set symbols:
-- ⋂ ⋃ ↑ ↓ ⫫ ∪ ∩ ⊆ ⊇
--
-- Greek alphabet (small):
-- α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ ς σ τ υ φ χ ψ ω
--
-- Miscellaneous:
-- ×ₑ ⊕ₑ ×ₐ ⊕ₐ
--

-- second-order variable type
type Var = Char


-- formula type
data Formula =
  FormulaAtomic String
  | Disj Formula Formula
  | Conj Formula Formula
  | Neg Formula
  | Exists Var Formula
  | ForAll Var Formula

-- prints the formula in human-readable format
showFormula :: Formula -> String
showFormula (FormulaAtomic phi) = phi
showFormula (Disj f1 f2)        = "(" ++ (showFormula f1) ++ ") ∨ (" ++ (showFormula f2) ++ ")"
showFormula (Conj f1 f2)        = "(" ++ (showFormula f1) ++ ") ∧ (" ++ (showFormula f2) ++ ")"
showFormula (Neg f)             = "¬(" ++ (showFormula f) ++ ")"
showFormula (Exists var f)      = "∃" ++ [var] ++ ". (" ++ (showFormula f) ++ ")"
showFormula (ForAll var f)      = "∀" ++ [var] ++ ". (" ++ (showFormula f) ++ ")"

-- instantiance of the data type as class Show
instance Show Formula where
	show = showFormula

-- -- an example formula with no meaning
-- exampleFormula :: Formula
-- exampleFormula = ForAll 'X' $ FormulaAtomic `Disj` (Exists 'Y' $ Neg (FormulaAtomic `Conj` (FormulaAtomic `Disj` FormulaAtomic)))

-- our example formula
exampleFormula :: Formula
exampleFormula =
  Exists 'X' $
  ForAll 'Y' $
  (FormulaAtomic "X ⊆ Y") `Conj`
    (Neg $ FormulaAtomic "X ⊇ Y") `Conj`
    (Exists 'Z' $ FormulaAtomic "Z = σ(Y)")

-- removes the universal quantifier
removeForAll :: Formula -> Formula
removeForAll (FormulaAtomic phi) = (FormulaAtomic phi)
removeForAll (Disj f1 f2)        = (Disj (removeForAll f1) (removeForAll f2))
removeForAll (Conj f1 f2)        = (Conj (removeForAll f1) (removeForAll f2))
removeForAll (Neg f)             = (Neg (removeForAll f))
removeForAll (Exists var f)      = (Exists var (removeForAll f))
removeForAll (ForAll var f)      = (Neg $ Exists var $ Neg (removeForAll f))


-- hierarchical automaton
data Aut =
  AutAtomic String
  | AutUnion Aut Aut
  | AutIsect Aut Aut
  | AutCompl Aut
  | AutProj Var Aut

-- prints the automaton in a human-readable format
showAut :: Aut -> String
showAut (AutAtomic phi)      = "aut{" ++ phi ++ "}"
showAut (AutUnion aut1 aut2) = "(" ++ (showAut aut1) ++ ") union (" ++ (showAut aut2) ++ ")"
showAut (AutIsect aut1 aut2) = "(" ++ (showAut aut1) ++ ") isect (" ++ (showAut aut2) ++ ")"
showAut (AutCompl aut)       = "compl(" ++ (showAut aut) ++ ")"
showAut (AutProj var aut)    = "proj[" ++ [var] ++ "](" ++ (showAut aut) ++ ")"

-- instantiance of the data type as class Show
instance Show Aut where
	show = showAut



-- translates a formula into an automaton
toAutomaton :: Formula -> Aut
toAutomaton (FormulaAtomic phi) = AutAtomic phi
toAutomaton (Disj f1 f2)        = AutUnion (toAutomaton f1) (toAutomaton f2)
toAutomaton (Conj f1 f2)        = AutIsect (toAutomaton f1) (toAutomaton f2)
toAutomaton (Neg f)             = AutCompl $ toAutomaton f
toAutomaton (Exists var f)      = AutProj var $ toAutomaton f
toAutomaton f@(ForAll _ _)      = toAutomaton $ removeForAll f


type State = Char
type FixpointFunc = String

-- fixpoint term type
data TermFix =
	TermFixSingleStateSet
	| TermFixSingleTermSet TermFix
	| TermFixCups TermFix TermFix
	| TermFixCupsProd TermFix TermFix
	| TermFixCaps TermFix TermFix
	| TermFixCapsProd TermFix TermFix
	| TermFixUnion TermFix TermFix
	| TermFixDownCl TermFix
	| TermFixUpClChoice TermFix
	| TermFixLFP FixpointFunc TermFix
	| TermFixGFP FixpointFunc TermFix

-- print out a fixpoint term in a human-readable format
showTermFix :: TermFix -> String
showTermFix TermFixSingleStateSet      = "{q}"
showTermFix (TermFixSingleTermSet t)   = "{" ++ (showTermFix t) ++ "}"
showTermFix (TermFixCups t1 t2)        = "(" ++ (showTermFix t1) ++ ") ×ₑ (" ++ (showTermFix t2) ++ ")"
showTermFix (TermFixCupsProd t1 t2)    = "(" ++ (showTermFix t1) ++ ") ⊕ₑ (" ++ (showTermFix t2) ++ ")"
showTermFix (TermFixCaps t1 t2)        = "(" ++ (showTermFix t1) ++ ") ×ₐ (" ++ (showTermFix t2) ++ ")"
showTermFix (TermFixCapsProd t1 t2)    = "(" ++ (showTermFix t1) ++ ") ⊕ₐ (" ++ (showTermFix t2) ++ ")"
showTermFix (TermFixUnion t1 t2)       = "(" ++ (showTermFix t1) ++ ") ∪ (" ++ (showTermFix t2) ++ ")"
showTermFix (TermFixDownCl t)          = "↓(" ++ (showTermFix t) ++ ")"
showTermFix (TermFixUpClChoice t)      = "↑⫫(" ++ (showTermFix t) ++ ")"
showTermFix (TermFixLFP func t)        = "μU. " ++ (showTermFix t) ++ " ∪ " ++ func ++ "(U)"
showTermFix (TermFixGFP func t)        = "νU. " ++ (showTermFix t) ++ " ∩ " ++ func ++ "(U)"

-- instantiance of the data type as class Show
instance Show TermFix where
	show = showTermFix


-- initial states
initial :: Aut -> TermFix
initial (AutAtomic phi)  = TermFixSingleStateSet
initial (AutUnion a1 a2) = (initial a1) `TermFixCups` (initial a2)
initial (AutIsect a1 a2) = (initial a1) `TermFixCaps` (initial a2)
initial (AutCompl a)     = TermFixSingleTermSet $ initial a
initial (AutProj var a)  = initial a

-- final states
final :: Aut -> TermFix
final (AutAtomic phi)  = TermFixSingleStateSet
final (AutUnion a1 a2) = TermFixCupsProd (final a1) (final a2)
final (AutIsect a1 a2) = TermFixCaps (final a1) (final a2)
final (AutCompl a)     = TermFixDownCl (nonfinal a)
final (AutProj var a)  = TermFixLFP ("pre[π(" ++ [var] ++ "), 0]") $ final a


-- nonfinal states
nonfinal :: Aut -> TermFix
nonfinal (AutAtomic phi)  = TermFixSingleStateSet
nonfinal (AutUnion a1 a2) = TermFixCups (nonfinal a1) (nonfinal a2)
nonfinal (AutIsect a1 a2) = TermFixCapsProd (nonfinal a1) (nonfinal a2)
nonfinal (AutCompl a)     = TermFixUpClChoice (final a)
nonfinal (AutProj var a)  = TermFixGFP ("cpre[π(" ++ [var] ++ "), 0]") $ nonfinal a




-- -- symbolic representation of a set of states
-- data SymbStateSet =
-- 	SymbStateSetAtomic
-- 	| SymbStateSetUnion SymbStateSet SymbStateSet
-- 	| SymbStateSetProd SymbStateSet SymbStateSet
-- 	| SymbStateSetEnclose SymbStateSet
-- 	| SymbStateSetDown SymbStateSet
-- 	| SymbStateSetUpChoice SymbStateSet
-- 	| SymbStateSetLFP Var SymbStateSet
-- 	| SymbStateSetGFP Var SymbStateSet


-- -- print out the symbolic state set in a human-readable format
-- showStateSet :: SymbStateSet -> String
-- showStateSet SymbStateSetAtomic        = "{states}"
-- showStateSet (SymbStateSetUnion s1 s2) = "(" ++ (showStateSet s1) ++ ") U (" ++ (showStateSet s2) ++ ")"
-- showStateSet (SymbStateSetProd s1 s2)  = "(" ++ (showStateSet s1) ++ ") x (" ++ (showStateSet s2) ++ ")"
-- showStateSet (SymbStateSetEnclose s)   = "{" ++ (showStateSet s) ++ "}"
-- showStateSet (SymbStateSetDown s)      = "down({" ++ (showStateSet s) ++ "})"
-- showStateSet (SymbStateSetUpChoice s)  = "upC({" ++ (showStateSet s) ++ "})"
-- showStateSet (SymbStateSetLFP var s)   = "uZ . " ++ (showStateSet s) ++ " union pre[" ++ (show var) ++ "](Z)"
-- showStateSet (SymbStateSetGFP var s)   = "nZ . " ++ (showStateSet s) ++ " isect cpre[" ++ (show var) ++ "](Z)"


-- -- instantiance of the data type as class Show
-- instance Show SymbStateSet where
-- 	show = showStateSet
--
--
-- -- final states
-- final :: Aut -> SymbStateSet
-- final AutAtomic        = SymbStateSetAtomic
-- final (AutUnion a1 a2) = SymbStateSetUnion (final a1) (final a2)
-- final (AutIsect a1 a2) = SymbStateSetProd (final a1) (final a2)
-- final (AutCompl a)     = SymbStateSetDown (nonfinal a)
-- final (AutProj var a)  = SymbStateSetLFP var $ final a
--
--
-- -- all states of an automaton
-- allstates :: Aut -> SymbStateSet
-- allstates AutAtomic    = SymbStateSetAtomic
--
--
