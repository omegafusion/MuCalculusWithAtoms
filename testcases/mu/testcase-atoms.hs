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
           formula1 = Mu vx (vx, Disjunction (Predicate pa) (Diamond (Variable vx)))
       in do
           P.print $ check [NL.constant 5] model formula1   -- {State 0 [], State 0 [5]}