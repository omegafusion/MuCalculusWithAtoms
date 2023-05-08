import Prelude hiding (filter, map, not, or, sum)
import qualified Prelude as P


import NLambda (Atom, Nominal, atom, constant, eq, variant, mapVariables, foldVariables)
import qualified NLambda as NL


import ModelCheckerUtils (State (..))
import ModelCheckerAtoms (check)

import Parser (parser)

import SyntaxUtils (
    Pred (..),
    boundedGraphRep)

import CTLSyntax (Formula (..))


--import Parser (parser)


main :: P.IO ()
main = 
    let a = constant 0
        b = constant 1
        c = constant 2
        d = constant 3
        mkstate i = NL.map (\a -> State i [a]) NL.atoms
        mkpred i = NL.map (\a -> Pred i [a]) NL.atoms
        s0star = State 0 []
        s0s = mkstate 0
        s1s = mkstate 1
        s2s = mkstate 2
        s3s = mkstate 3
        p0s = mkpred 0
        p1s = mkpred 1
        p2s = mkpred 2
        p3s = mkpred 3
        myStates = NL.insert s0star (NL.unions [s0s, s1s, s2s, s3s])
        mktrans i j = NL.map (\a -> (State i [a], State j [a])) NL.atoms
        myTrans = NL.union (NL.map (\a -> (s0star, State 0 [a])) NL.atoms) (NL.unions [mktrans 0 1,
                             mktrans 1 2,
                             mktrans 2 0,
                             mktrans 0 3,
                             mktrans 3 3])
        mksat i = NL.map (\a -> (State i [a], Pred i [a])) NL.atoms
        mySat = NL.unions [mksat 0,
                           mksat 1,
                           mksat 2,
                           mksat 3]
        myKripkeStructure = (myStates, myTrans, mySat)

        myFormula = parser "C [ |_a . E X p0_a ]"
        myFormulaExpected = P.Right $ IndexedDisjunction (boundedGraphRep 1 (\[a] -> ExistsNext (Predicate (Pred 0 [a]))))
    in do
        print $ myFormula `NL.eq` myFormulaExpected
        print $ check myKripkeStructure myFormulaExpected `NL.eq` NL.union (NL.singleton (State 0 [])) (NL.map (\a -> State 2 [a]) NL.atoms)
