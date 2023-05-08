-- a testcase for one of the examples in the paper

import Prelude (($), (==), Either (..))
import qualified Prelude as P

import qualified NLambda as NL

import ModelCheckerUtils (
    State (..),
    TransRel,
    SatRel,
    KripkeModel)

import Data.Either (fromLeft)

import ModelCheckerAtoms (check)

import Parser (parser)

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
           indexedDisjunctionPart1 a = IndexedDisjunction ([0], NL.map (\b -> ([b], Variable (Var 0 [b]))) (NL.atoms `NL.difference` NL.singleton a))
           row1 = ([0], NL.map (\a -> ([a], Negation (Disjunction (Negation (Predicate (Pred 0 [a]))) (Negation (indexedDisjunctionPart1 a))))) NL.atoms)
           formula1 = parser "M[ mu v0_0 { a . p0_a & (|_b/=a . v0_b) } ]"
           formula1Expected = Left $ Mu x row1
           indexedDisjunctionPart2 a = IndexedDisjunction ([0], NL.map (\b -> ([b], Negation (Variable (Var 0 [b])))) (NL.atoms `NL.difference` NL.singleton a))
           row2 = ([0], NL.map (\a -> ([a], Negation (Disjunction (Negation (Predicate (Pred 0 [a]))) (Negation (indexedDisjunctionPart2 a))))) NL.atoms)
           formula2 = parser "M[ mu v0_0 { a . p0_a & (|_b/=a . ~v0_b) } ]"
           formula2Expected = Left $ Mu x row2
           formula3 = parser "M[ mu v0_7 { a . p0_a & (|_b/=a . ~v0_b) } ]"
           formula3Expected = Left $ Mu (Var 0 [NL.constant 7]) row2
       in do
           P.print $ check model formula1 `NL.eq` NL.empty;    -- empty
           P.print $ formula1 `NL.eq` formula1Expected;
           P.print $ check model formula2 `NL.eq` NL.singleton (State 0 [NL.constant 0]);    -- {state 0 [0]}
           P.print $ formula2 `NL.eq` formula2Expected;
           P.print $ check model formula3 `NL.eq` NL.singleton (State 0 [NL.constant 7]);    -- {state 0 [7]}
           P.print $ formula3 `NL.eq` formula3Expected;