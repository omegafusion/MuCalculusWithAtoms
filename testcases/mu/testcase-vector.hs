-- a testcase for one of the examples in the paper

import Prelude (($))
import qualified Prelude as P

import qualified NLambda as NL

import ModelCheckerUtils (
    State (..),
    TransRel,
    SatRel,
    KripkeModel)

import MuModelCheckerAtoms (check)


import SyntaxUtils (Pred (..))

import MuSyntax (
    Formula (..),
    Var (..))


main :: P.IO()
main = let states = NL.map (\a -> State 0 [a]) NL.atoms
           transRel = NL.map (\a -> (a, a)) states
           satRel = NL.map (\a -> (State 0 [a], Pred 0 [a])) NL.atoms
           model = (states, transRel, satRel)

           x = Var 0 [NL.constant 0]
           -- No other state has the same predicate
           a = NL.atom "a"
           indexedDisjunctionPart = IndexedDisjunction (NL.map (\b -> (b, Variable (Var 0 [b]))) (NL.atoms `NL.difference` NL.singleton a))
           row1 = (Var 0 [a], Negation (Disjunction (Negation (Predicate (Pred 0 [a]))) (Negation indexedDisjunctionPart)))
           formula1 = Mu x row1
           row2 = (Var 0 [a], Negation (Disjunction (Negation (Predicate (Pred 0 [a]))) indexedDisjunctionPart))
           formula2 = Mu x row2
           formula3 = Mu (Var 0 [NL.constant 7]) row2
       in do
           P.print $ check [] model formula1    -- empty
           P.print $ check [] model formula2    -- {state 0 [0]}
           P.print $ check [] model formula3    -- {state 0 [7]}