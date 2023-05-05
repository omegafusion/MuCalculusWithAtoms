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


main :: P.IO ()
main = 
    let a = constant 0
        b = constant 1
        c = constant 2
        d = constant 3
        mkstate i = NL.orbit [] (State i [atom "a"])
        mkpred i = NL.orbit [] (Pred i [atom "a"])
        s0star = State 0 []
        s0s = mkstate 0
        s1s = mkstate 1
        s2s = mkstate 2
        s3s = mkstate 3
        p0s = mkpred 0
        p1s = mkpred 1
        p2s = mkpred 2
        p3s = mkpred 3
        myStates :: NL.Set State
        myStates = NL.insert s0star (NL.unions [s0s, s1s, s2s, s3s])
        mktrans i j = NL.orbit [] (State i [atom "a"], State j [atom "a"])
        myTrans :: TransRel
        myTrans = NL.union (NL.orbit [] (s0star, State 0 [atom "a"])) (NL.unions [mktrans 0 1,
                             mktrans 1 2,
                             mktrans 2 0,
                             mktrans 0 3,
                             mktrans 3 3])
        mksat i = NL.orbit [] (State i [atom "a"], Pred i [atom "a"])
        mySat :: SatRel
        mySat = NL.unions [mksat 0,
                           mksat 1,
                           mksat 2,
                           mksat 3]
        myKripkeStructure = (myStates, myTrans, mySat)
        vx = Var 0 []
        vy = Var 1 []
        -- is a state with Pred a reachable?
        myFormula = parser "M[ |_a . mu v0 . p0_a | <> v0 ]"
        myFormulaExpected = Left $ IndexedDisjunction ([], NL.map (\a -> ([a], Mu vx ([0], constantAsGraph (Disjunction (Predicate (Pred 0 [a])) (Diamond (Variable vx)))))) NL.atoms)
        --myFormula = parser "mu v0 . p0 | <>v0"
        --myFormulaExpected = Mu vx (Disjunction (Predicate p0) (Diamond (Variable vx)))
        -- is a state with Pred d reachable?
        --myFormula2 = parser "mu v1 . p3 | <>v1"
        --myFormula2Expected = Mu vy (Disjunction (Predicate p3) (Diamond (Variable vy)))
        -- not Pred a and not Pred b
        --myFormula3 = parser "~(p0 | p1)"
        --myFormula3Expected = Negation (Disjunction (Predicate p0) (Predicate p1))
    in do
        --print $ check myKripkeStructure myFormula   -- [a, b, c]
        --print $ myFormula == myFormulaExpected
        --print $ check myKripkeStructure myFormula2  -- [a, b, c, d]
        --print $ myFormula2 == myFormula2Expected
        --print $ check myKripkeStructure myFormula3  -- [c, d]
        --print $ myFormula3 == myFormula3Expected
        print $ check myKripkeStructure myFormulaExpected   -- [s0, s0_a, s1_a, s2_a for each a]
        print $ myFormula == myFormulaExpected