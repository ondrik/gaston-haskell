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


-- --------------------------------------------------------------------------
-- part with LaTeX rendering
-- --------------------------------------------------------------------------

texOr     = "\\lor"
texAnd    = "\\land"
texNot    = "\\neg"
texExists = "\\exists"
texForAll = "\\forall"

-- translates a formula to TeX
formulaToTex :: Formula -> String
formulaToTex (FormulaAtomic phi)
  | phi == "X ⊆ Y"    = "X \\subseteq Y"
  | phi == "X ⊇ Y"    = "X \\supseteq Y"
  | phi == "Z = σ(Y)" = "Z = \\sigma(Y)"
  | otherwise         = error "initial: Unknown atomic predicate"
formulaToTex (Disj f1 f2)   = binaryOpTex texOr f1 f2
formulaToTex (Conj f1 f2)   = binaryOpTex texAnd f1 f2
formulaToTex (Neg f)        = texNot ++ (parenthesiseTex f)
formulaToTex (Exists var f) = quantifierTex texExists var f
formulaToTex (ForAll var f) = quantifierTex texForAll var f


parenthesiseTex :: Formula -> String
parenthesiseTex str = "\\left( " ++ (formulaToTex str) ++ " \\right)"


binaryOpTex :: String -> Formula -> Formula -> String
binaryOpTex op f1 f2 =
  (parenthesiseTex f1) ++
  " " ++ op ++ " " ++
  (parenthesiseTex f2)


quantifierTex :: String -> Var -> Formula -> String
quantifierTex quant var f = quant ++ " " ++ [var] ++ " " ++ (parenthesiseTex f)
