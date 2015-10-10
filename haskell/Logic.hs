module Logic where

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


-- removes the universal quantifier
removeForAll :: Formula -> Formula
removeForAll (FormulaAtomic phi) = (FormulaAtomic phi)
removeForAll (Disj f1 f2)        = (Disj (removeForAll f1) (removeForAll f2))
removeForAll (Conj f1 f2)        = (Conj (removeForAll f1) (removeForAll f2))
removeForAll (Neg f)             = (Neg (removeForAll f))
removeForAll (Exists var f)      = (Exists var (removeForAll f))
removeForAll (ForAll var f)      = (Neg $ Exists var $ Neg (removeForAll f))


-- our example formula
exampleFormula :: Formula
exampleFormula =
  Exists 'X' $
  ForAll 'Y' $
  (FormulaAtomic "X ⊆ Y") `Conj`
    (Neg $ FormulaAtomic "X ⊇ Y") `Conj`
    (Exists 'Z' $ FormulaAtomic "Z = σ(Y)")


-- help
helpLines :: [String]
helpLines = [
  "exampleFormula :: String    -- the formula " ++ (showFormula exampleFormula),
  ""
  ]
