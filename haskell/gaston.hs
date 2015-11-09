--
-- Gaston: A prototype implementation of a new decision procedure for WS1S
--

import qualified Logic
import Logic (exampleFormula)

import qualified Automaton
import Automaton (exampleAutomaton)

import qualified GUIDisplay
import GUIDisplay (displayFormula)


-- type State = Char
-- type FixpointFunc = String
--
-- -- fixpoint term type
-- data TermFix =
-- 	TermFixSingleStateSet
-- 	| TermFixSingleTermSet TermFix
-- 	| TermFixCups TermFix TermFix
-- 	| TermFixCupsProd TermFix TermFix
-- 	| TermFixCaps TermFix TermFix
-- 	| TermFixCapsProd TermFix TermFix
-- 	| TermFixUnion TermFix TermFix
-- 	| TermFixDownCl TermFix
-- 	| TermFixUpClChoice TermFix
-- 	| TermFixLFP FixpointFunc TermFix
-- 	| TermFixGFP FixpointFunc TermFix
--
-- -- print out a fixpoint term in a human-readable format
-- showTermFix :: TermFix -> String
-- showTermFix TermFixSingleStateSet      = "{q}"
-- showTermFix (TermFixSingleTermSet t)   = "{" ++ (showTermFix t) ++ "}"
-- showTermFix (TermFixCups t1 t2)        = "(" ++ (showTermFix t1) ++ ") ×ₑ (" ++ (showTermFix t2) ++ ")"
-- showTermFix (TermFixCupsProd t1 t2)    = "(" ++ (showTermFix t1) ++ ") ⊕ₑ (" ++ (showTermFix t2) ++ ")"
-- showTermFix (TermFixCaps t1 t2)        = "(" ++ (showTermFix t1) ++ ") ×ₐ (" ++ (showTermFix t2) ++ ")"
-- showTermFix (TermFixCapsProd t1 t2)    = "(" ++ (showTermFix t1) ++ ") ⊕ₐ (" ++ (showTermFix t2) ++ ")"
-- showTermFix (TermFixUnion t1 t2)       = "(" ++ (showTermFix t1) ++ ") ∪ (" ++ (showTermFix t2) ++ ")"
-- showTermFix (TermFixDownCl t)          = "↓(" ++ (showTermFix t) ++ ")"
-- showTermFix (TermFixUpClChoice t)      = "↑⫫(" ++ (showTermFix t) ++ ")"
-- showTermFix (TermFixLFP func t)        = "μU. " ++ (showTermFix t) ++ " ∪ " ++ func ++ "(U)"
-- showTermFix (TermFixGFP func t)        = "νU. " ++ (showTermFix t) ++ " ∩ " ++ func ++ "(U)"
--
-- -- instantiance of the data type as class Show
-- instance Show TermFix where
-- 	show = showTermFix


runExample :: Automaton.ReturnVal
runExample = Automaton.isectNonempty
               exampleAutomaton
               (Automaton.initial exampleAutomaton)
               (Automaton.final exampleAutomaton)

-- help
gastonHelpLines :: [String]
gastonHelpLines = [
  "runExample :: ReturnVal      -- runs the example",
  ""
  ]

helpLines :: [String]
helpLines = [
  "Logic:",
  ""
  ]
  ++ (offsetBy 2 Logic.helpLines)
  ++ [
  "Automaton:",
  ""
  ]
  ++ (offsetBy 2 Automaton.helpLines)
  ++ [
  "gaston:",
  ""
  ]
  ++ (offsetBy 2 gastonHelpLines)
  ++ []   -- guard
  where
    offsetBy n [] = []
    offsetBy n (x:xs) = ((replicate n ' ') ++ x):(offsetBy n xs)

help :: IO ()
help = putStrLn $ unlines helpLines
