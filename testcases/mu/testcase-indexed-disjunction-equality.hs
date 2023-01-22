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


import MuModelCheckerAtoms (check)


import SyntaxUtils ( Pred (..) )

import MuSyntax (
    Formula (..),
    Var (..),
    graphRep)


import Parser (parser)


main :: P.IO ()
main = 
    let myStates1 = NL.map (\a -> State 0 [a]) NL.atoms
        myTrans1 = NL.map (\a -> (a, a)) myStates1
        mySat1 = NL.map (\a -> (State 0 [a], Pred 0 [a])) NL.atoms
        myKripkeStructure1 = (myStates1, myTrans1, mySat1)

        mySat2 = NL.pairs myStates1 (NL.map (\a -> Pred 0 [a]) NL.atoms)
        myKripkeStructure2 = (myStates1, myTrans1, mySat2)

        -- for some atom, the states without any predicates labelled with atoms other than that atom
        myFormula = parser "|_a . &_b/=a .  ~p0_b"
       -- myFormulaExpected = ???
    in do
        print $ check myKripkeStructure1 myFormula
        print $ check myKripkeStructure2 myFormula
