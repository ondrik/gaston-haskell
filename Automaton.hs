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
    STSet [State]
  | STUnionFin StateTerm StateTerm
  | STUnionNonfin StateTerm StateTerm
  | STIsectFin StateTerm StateTerm
  | STIsectNonfin StateTerm StateTerm
  | STUpClosed StateTerm
  | STUpClosedChoice StateTerm
  | STDownClosed StateTerm
  | STUnknown


-- print out a state term in a human-readable format
showST :: StateTerm -> String
showST (STSet xs)            = "\ESC[34m" ++ (show xs) ++ "\ESC[m"
showST (STUnionFin t1 t2)    = "(" ++ (showST t1) ++ " ᵤ×ᶠ " ++ (showST t2) ++ ")"
showST (STUnionNonfin t1 t2) = "(" ++ (showST t1) ++ " ᵤ×ⁿ " ++ (showST t2) ++ ")"
showST (STIsectFin t1 t2)    = "(" ++ (showST t1) ++ " ᵢ×ᶠ " ++ (showST t2) ++ ")"
showST (STIsectNonfin t1 t2) = "(" ++ (showST t1) ++ " ᵢ×ⁿ " ++ (showST t2) ++ ")"
showST (STUpClosed t)        = "↑{" ++ showST t ++ "}"
showST (STUpClosedChoice t)  = "\ESC[33m↑⫫{\ESC[m" ++ showST t ++ "\ESC[33m}\ESC[m"
showST (STDownClosed t)      = "\ESC[35m↓{\ESC[m" ++ showST t ++ "\ESC[35m}\ESC[m"
showST STUnknown             = "\ESC[31m??\ESC[m"

-- instantiance of the data type as class Show
instance Show StateTerm where
  show = showST


-- initial states
initial :: Aut -> StateTerm
initial (AutAtomicFin phi)
  | phi == "X ⊆ Y"    = STSet ["q1"]
  | phi == "X ⊇ Y"    = STSet ["q2"]
  | phi == "Z = σ(Y)" = STSet ["q3"]
  | otherwise         = error "initial: Unknown atomic predicate"
initial (AutAtomicNonfin phi)
  | phi == "X ⊆ Y"    = STSet ["q1"]
  | phi == "X ⊇ Y"    = STSet ["q2"]
  | phi == "Z = σ(Y)" = STSet ["q3"]
  | otherwise         = error "initial: Unknown atomic predicate"
initial (AutUnionFin a1 a2)    = (initial a1) `STUnionFin` (initial a2)
initial (AutUnionNonfin a1 a2) = (initial a1) `STUnionNonfin` (initial a2)
initial (AutIsectFin a1 a2)    = (initial a1) `STIsectFin` (initial a2)
initial (AutIsectNonfin a1 a2) = (initial a1) `STIsectNonfin` (initial a2)
initial (AutComplFin a)        = STUpClosed $ initial a
initial (AutComplNonfin a)     = STUpClosed $ initial a
initial (AutProjFin var a)     = initial a
initial (AutProjNonfin var a)  = initial a


-- final states
final :: Aut -> StateTerm
final (AutAtomicFin phi)
  | phi == "X ⊆ Y"    = STSet ["q1"]
  | phi == "X ⊇ Y"    = STSet ["q2"]
  | phi == "Z = σ(Y)" = STSet ["q3"]
  | otherwise         = error "final: Unknown atomic predicate"
final (AutUnionFin a1 a2)    = (final a1) `STUnionFin` (final a2)
final (AutIsectFin a1 a2)    = (final a1) `STIsectFin` (final a2)
final (AutComplFin a)        = STDownClosed $ nonfinal a
final (AutProjFin var a)     = final a           -- FIXME: this is clearly wrong


-- nonfinal states
nonfinal :: Aut -> StateTerm
nonfinal (AutAtomicNonfin phi)
  | phi == "X ⊆ Y"    = STSet []
  | phi == "X ⊇ Y"    = STSet []
  | phi == "Z = σ(Y)" = STSet ["q4"]
  | otherwise         = error "nonfinal: Unknown atomic predicate"
nonfinal (AutUnionNonfin a1 a2) = (nonfinal a1) `STUnionNonfin` (nonfinal a2)
nonfinal (AutIsectNonfin a1 a2) = (nonfinal a1) `STIsectNonfin` (nonfinal a2)
nonfinal (AutComplNonfin a)     = STUpClosedChoice $ final a
nonfinal (AutProjNonfin var a)  = nonfinal a     -- FIXME: this is clearly wrong


-- zero-predecessors of a state term with a transition function with given variables projected out
pre :: Aut -> [Var] -> StateTerm -> StateTerm
pre (AutUnionFin a1 a2) vars (STUnionFin t1 t2) = (pre a1 vars t1) `STUnionFin` (pre a2 vars t2)
pre (AutIsectFin a1 a2) vars (STIsectFin t1 t2) = (pre a1 vars t1) `STIsectFin` (pre a2 vars t2)
pre (AutComplFin a) vars (STDownClosed t)       = STDownClosed (cpre a vars t)
pre (AutProjFin var a) vars t                   = pre a vars t        -- FIXME: this is clearly wrong
pre (AutAtomicFin phi) vars t
  | phi == "X ⊆ Y"    = error $ "pre(X ⊆ Y); vars = " ++ (show vars) ++ "; t = " ++ (show t)
  | phi == "X ⊇ Y"    = error $ "pre(X ⊇ Y)" ++ (show vars) ++ "; t = " ++ (show t)
  | phi == "Z = σ(Y)" = error $ "pre(Z = σ(Y))" ++ (show vars) ++ "; t = " ++ (show t)
  | otherwise         = error "pre: Unknown atomic predicate"
pre aut vars t =
  error $ "Invalid input of pre: aut = " ++ (show aut) ++ "; vars = " ++ (show vars) ++ "; t = " ++ (show t)


-- zero-(controllable) predecessors of a state term with a transition function with given variables projected out
cpre :: Aut -> [Var] -> StateTerm -> StateTerm
cpre (AutUnionNonfin a1 a2) vars (STUnionNonfin t1 t2) = (cpre a1 vars t1) `STUnionNonfin` (cpre a2 vars t2)
cpre (AutIsectNonfin a1 a2) vars (STIsectNonfin t1 t2) = (cpre a1 vars t1) `STIsectNonfin` (cpre a2 vars t2)
cpre (AutComplNonfin a) vars (STUpClosedChoice t)      = STUpClosedChoice (pre a vars t)
cpre (AutProjNonfin var a) vars t                      = cpre a vars t        -- FIXME: this is clearly wrong
cpre (AutAtomicNonfin phi) vars t
  | phi == "X ⊆ Y"    = error $ "cpre(X ⊆ Y)" ++ (show vars) ++ "; t = " ++ (show t)
  | phi == "X ⊇ Y"    = error $ "cpre(X ⊇ Y)" ++ (show vars) ++ "; t = " ++ (show t)
  | phi == "Z = σ(Y)" = error $ "cpre(Z = σ(Y))" ++ (show vars) ++ "; t = " ++ (show t)
  | otherwise         = error "cpre: Unknown atomic predicate"
cpre aut vars t =
  error $ "Invalid input of cpre: aut = " ++ (show aut) ++ "; vars = " ++ (show vars) ++ "; t = " ++ (show t)


-- used as a return value of evaluation functions
type ReturnVal = (Bool, StateTerm)


-- checks whether the intersection of a pair of lists is nonempty
listIsectNonempty :: Eq a => [a] -> [a] -> Bool
listIsectNonempty lhs rhs = or $ map (\x -> x `elem` rhs) lhs


-- tests whether an intersection of state terms is nonempty
isectNonempty :: Aut -> StateTerm -> StateTerm -> ReturnVal
isectNonempty (AutUnionFin a1 a2) (STUnionFin l1 l2) (STUnionFin r1 r2) = res
  where
    (bool1, fxp1) = isectNonempty a1 l1 r1
    (bool2, fxp2) = isectNonempty a2 l2 r2
    res = case (bool1, bool2) of
      (False, False) -> (False, fxp1 `STUnionFin` fxp2)
      (True, _)      -> (True, fxp1 `STUnionFin` STUnknown)
      (_, True)      -> (True, STUnknown `STUnionFin` fxp2)
isectNonempty (AutIsectFin a1 a2) (STIsectFin l1 l2) (STIsectFin r1 r2) = res
  where
    (bool1, fxp1) = isectNonempty a1 l1 r1
    (bool2, fxp2) = isectNonempty a2 l2 r2
    res = case (bool1, bool2) of
      (True, True) -> (True, fxp1 `STIsectFin` fxp2)
      (False, _)   -> (False, fxp1 `STIsectFin` STUnknown)
      (_, False)   -> (False, STUnknown `STIsectFin` fxp2)
isectNonempty (AutComplFin a) (STUpClosed lhs) (STDownClosed rhs)       = (fst res, STDownClosed (snd res))
  where
    res = isSubset a lhs rhs
isectNonempty (AutProjFin var a) lhs rhs                                = isectNonempty a lhs rhs        -- FIXME: this is wrong
isectNonempty (AutAtomicFin _) (STSet lhs) (STSet rhs)                  = (listIsectNonempty lhs rhs, STSet rhs)
isectNonempty aut lhs rhs =
  error $ "isectNonempty: incompatible terms in aut = " ++ (show aut) ++ "; lhs = " ++ (show lhs) ++ "; rhs = " ++ (show rhs)

-- checks whether one list is a subset of another list
listIsSubset :: Eq a => [a] -> [a] -> Bool
listIsSubset lhs rhs = and $ map (\x -> x `elem` rhs) lhs


-- tests whether one state term is (semantically) a subset of another one
isSubset :: Aut -> StateTerm -> StateTerm -> ReturnVal
isSubset (AutUnionNonfin a1 a2) (STUnionNonfin l1 l2) (STUnionNonfin r1 r2) = ((fst res1) || (fst res2), (snd res1) `STUnionNonfin` (snd res2))
  where
    res1 = isSubset a1 l1 r1
    res2 = isSubset a2 l2 r2
isSubset (AutIsectNonfin a1 a2) (STIsectNonfin l1 l2) (STIsectNonfin r1 r2) = ((fst res1) && (fst res2), (snd res1) `STIsectNonfin` (snd res2))
  where
    res1 = isSubset a1 l1 r1
    res2 = isSubset a2 l2 r2
isSubset (AutComplNonfin a) (STUpClosed lhs) (STUpClosedChoice rhs)         = (fst res, STUpClosedChoice (snd res))
  where
    res = isectNonempty a lhs rhs
isSubset (AutProjNonfin var a) lhs rhs                                      = isSubset a lhs rhs        -- FIXME: this is wrong
isSubset (AutAtomicNonfin _) (STSet lhs) (STSet rhs)                        = (listIsSubset lhs rhs, STSet rhs)
isSubset aut lhs rhs =
  error $ "isSubset: incompatible terms in aut = " ++ (show aut) ++ "; lhs = " ++ (show lhs) ++ "; rhs = " ++ (show rhs)


-- the automaton for the example formula
exampleAutomaton :: Aut
exampleAutomaton = toAutomaton Logic.exampleFormula

