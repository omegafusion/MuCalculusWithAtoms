-- a testcase for one of the examples in the paper

import Prelude (($), (==), Either (..), const)
import qualified Prelude as P

import qualified NLambda as NL

import ModelCheckerUtils (
    State (..),
    TransRel,
    SatRel,
    KripkeModel)

import ModelCheckerAtoms (check)

import Parser (parser)

import SyntaxUtils (
    Pred (..),
    constantAsGraph)

import MuSyntax (
    Formula (..),
    Var (..))


main :: P.IO()
main = let states = NL.insert (State 0 []) (NL.map (\a -> State 0 [a]) NL.atoms)
           transRel = NL.unions [NL.map (\a -> (State 0 [], State 0 [a])) NL.atoms, NL.map (\a -> (State 0 [a], State 0 [a])) NL.atoms]
           satRel = NL.map (\a -> (State 0 [a], Pred 0 [a])) NL.atoms
           model = (states, transRel, satRel)

           vx = Var 0 []
           pa = Pred 0 [NL.constant 5]
           formula1 = parser "M[ mu v0 . p0_5 | <>v0 ]"
           formula1Expected = Left $ Mu vx ([0], constantAsGraph (Disjunction (Predicate pa) (Diamond (Variable vx))))
       in do
           P.print $ check model formula1 `NL.eq` NL.fromList [State 0 [], State 0 [NL.constant 5]]   -- {State 0 [], State 0 [5]}
           P.print $ formula1 `NL.eq` formula1Expected
