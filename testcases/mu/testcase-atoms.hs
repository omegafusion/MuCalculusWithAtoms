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

import SyntaxUtils (Pred (..))

import MuSyntax (
    Formula (..),
    Var (..),
    graphRep)


main :: P.IO()
main = let states :: NL.Set State
           states = NL.insert (State 0 []) (NL.map (\a -> State 0 [a]) NL.atoms)
           transRel :: TransRel
           transRel = NL.unions [NL.map (\a -> (State 0 [], State 0 [a])) NL.atoms, NL.map (\a -> (State 0 [a], State 0 [a])) NL.atoms]
           satRel :: SatRel
           satRel = NL.map (\a -> (State 0 [a], Pred 0 [a])) NL.atoms
           model :: KripkeModel
           model = (states, transRel, satRel)

           vx = Var 0 []
           pa = Pred 0 [NL.constant 5]
           formula1 = parser "M[ mu v0 . p0_5 | <>v0 ]"
           formula1Expected = Left $ MuV vx ([0], NL.singleton ([], Disjunction (Predicate pa) (Diamond (Variable vx))))
       in do
           P.print $ check [NL.constant 5] model formula1   -- {State 0 [], State 0 [5]}
           P.print $ formula1 == formula1Expected
