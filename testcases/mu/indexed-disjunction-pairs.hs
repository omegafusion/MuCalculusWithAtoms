import Prelude hiding (filter, map, not, or, sum)
import qualified Prelude as P


import NLambda (Atom, Nominal, atom, constant, eq, variant, mapVariables, foldVariables)
import qualified NLambda as NL


import ModelCheckerUtils (
    State (..),
    TransRel,
    SatRel,
    KripkeModel)


import ModelCheckerAtoms (check)


import SyntaxUtils (
    Pred (..),
    graphRep,
    constantAsGraph)

import MuSyntax (
    Formula (..),
    Var (..))


import Parser (parser)



states = NL.map (State 0) (NL.replicateAtomsUntil 2)
trans = NL.map (\[a,b] -> (State 0 [a], State 0 [a,b])) (NL.replicateAtoms 2)
        `NL.union` NL.map (\[a] -> (State 0 [], State 0 [a])) (NL.replicateAtoms 1)
        `NL.union` NL.map (\as -> (State 0 as, State 0 as)) (NL.replicateAtoms 2)
sat = NL.map (\as -> (State 0 as, Pred 0 as)) (NL.replicateAtomsUntil 2)
myKripkeStructure = (states, trans, sat)

myFormula1 = parser "M[ |_a . |_b . mu v0 . p0_a,b | <> v0 ]"
myFormula2 = parser "M[ |_a . mu v0 . p0_a,a | <> v0 ]"

f3 = parser "M [ p0_0,0 ]"
f3' = Predicate (Pred 0 [constant 0, constant 0])

main :: P.IO ()
main = 
    let vx = Var 0 []
        vy = Var 1 []
    in do
        print $ check myKripkeStructure myFormula1 `NL.eq` states
        --print $ myFormula2
        print $ check myKripkeStructure myFormula2 `NL.eq` NL.union (NL.singleton (State 0 [])) (NL.unions [NL.map (\a -> State 0 [a]) NL.atoms, NL.map (\as -> State 0 as) $ NL.replicateAtoms 2])
