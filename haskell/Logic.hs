module Logic where

import Data.List


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


-- retrieves free variables of a formula
freeVars :: Formula -> [Var]
freeVars (FormulaAtomic phi)
  | phi == "X ⊆ Y"    = ['X', 'Y']
  | phi == "X ⊇ Y"    = ['X', 'Y']
  | phi == "Z = σ(Y)" = ['Z', 'Y']
  | otherwise         = error "initial: Unknown atomic predicate"
freeVars (Disj f1 f2)   = nub $ (freeVars f1) ++ (freeVars f2)
freeVars (Conj f1 f2)   = freeVars (Disj f1 f2)
freeVars (Neg f)        = freeVars f
freeVars (Exists var f) = delete var $ freeVars f
freeVars (ForAll var f) = freeVars (Exists var f)


antiprenex :: Formula -> Formula
antiprenex f@(FormulaAtomic _) = f
antiprenex (Disj f1 f2)        = Disj (antiprenex f1) (antiprenex f2)
antiprenex (Conj f1 f2)        = Conj (antiprenex f1) (antiprenex f2)
antiprenex (Neg f)             = Neg (antiprenex f)
antiprenex (Exists var f) =
  case f of
    Disj g1 g2 -> (Exists var $ antiprenex g1) `Disj` (Exists var $ antiprenex g2)
    _          -> Exists var $ antiprenex f
      -- where
      --   fvF1 = g1
      --   fvF2 = g2
antiprenex phi@(ForAll var f) =
  case f of
    Conj g1 g2 -> (ForAll var $ antiprenex g1) `Conj` (ForAll var $ antiprenex g2)
    _          -> ForAll var $ antiprenex f



-- our example formula
exampleFormula :: Formula
exampleFormula =
  Exists 'X' $
  ForAll 'Y' $
  (FormulaAtomic "X ⊆ Y") `Conj`
    (Neg $ FormulaAtomic "X ⊇ Y") `Conj`
    (Exists 'Z' $ FormulaAtomic "Z = σ(Y)")

exampleFormulaAntiprenexed :: Formula
exampleFormulaAntiprenexed = antiprenex exampleFormula


-- help
helpLines :: [String]
helpLines = [
  "exampleFormula :: String    -- the formula " ++ (showFormula exampleFormula),
  ""
  ]


-- --------------------------------------------------------------------------
-- part with LaTeX rendering
-- --------------------------------------------------------------------------

texOr     :: String
texOr      = "\\lor"

texAnd    :: String
texAnd     = "\\land"

texNot    :: String
texNot     = "\\neg"

texExists :: String
texExists  = "\\exists"

texForAll :: String
texForAll  = "\\forall"


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


closeInParentheses :: Formula -> String
closeInParentheses f = "\\left( " ++ (formulaToTex f) ++ " \\right)"

parenthesiseTex :: Formula -> String
parenthesiseTex f = if shouldParent f then closeInParentheses f else " " ++ (formulaToTex f) ++ " "


binaryOpTex :: String -> Formula -> Formula -> String
binaryOpTex op f1 f2 =
  (parenthesiseTex f1) ++
  " " ++ op ++ " " ++
  (parenthesiseTex f2)


quantifierTex :: String -> Var -> Formula -> String
quantifierTex quant var f = quant ++ " " ++ [var] ++ " " ++
  (if isQuantifier f then formulaToTex f else closeInParentheses f)


-- True iff the formula begins with a quantifier
isQuantifier :: Formula -> Bool
isQuantifier (Exists _ _) = True
isQuantifier (ForAll _ _) = True
isQuantifier _ = False


-- True iff the formula should be parenthesised
shouldParent :: Formula -> Bool
shouldParent (Disj _ _)     = True
shouldParent (Conj _ _)     = True
shouldParent _ = False
