module Main where
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
    graphRep)

import MuSyntax (
    Formula (..),
    Var (..))


import Parser (parser)


main :: P.IO ()
main = 
    let myStates1 = NL.map (\a -> State 0 [a]) NL.atoms
        myTrans1 = NL.map (\a -> (a, a)) myStates1
        mySat1 = NL.map (\a -> (State 0 [a], Pred 0 [a])) NL.atoms
        myKripkeStructure1 = (myStates1, myTrans1, mySat1)

        mySat2 = NL.pairs myStates1 (NL.map (\a -> Pred 0 [a]) NL.atoms)
        myKripkeStructure2 = (myStates1, myTrans1, mySat2)

        myStates3 = NL.filter (\(State 0 [a]) -> a `NL.ge` constant 0) myStates1    -- states with atoms >=0
        myTrans3 = NL.map (\a -> (a, a)) myStates3
        mySat3 = NL.filter (\(State 0 [a], Pred 0 [b]) -> a `NL.ge` b NL./\ b `NL.ge` constant 0) mySat2 -- states have all preds with smaller (and >=0) atoms
        myKripkeStructure3 = (myStates3, myTrans3, mySat3)

        myKripkeStructure4 = (myStates1, myTrans1, mySat3)

        -- for some atom, the states without any predicates labelled with atoms other than that atom
        myFormula1 = parser "M[ |_a . &_b/=a .  ~p0_b ]"
        myFormula2 = parser "M[ |_a . &_b<a .  ~p0_b ]"
       -- myFormulaExpected = ???
    in do
        print $ check myKripkeStructure1 myFormula1 -- all states
        print $ check myKripkeStructure1 myFormula2 -- all states
        print $ check myKripkeStructure2 myFormula1 -- no states
        print $ check myKripkeStructure2 myFormula2 -- no states
        print $ check myKripkeStructure3 myFormula1 -- { State 0 [0] }
        print $ check myKripkeStructure3 myFormula2 -- { State 0 [a] : a >= 0 } i.e. the entire state space for this example
        print $ check myKripkeStructure4 myFormula1 -- { State 0 [a] : a <= 0 }
        print $ check myKripkeStructure4 myFormula2 -- all states
