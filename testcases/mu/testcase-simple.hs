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

import SyntaxUtils (Pred (..))

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
        s0 = State 0 []
        s1 = State 1 []
        s2 = State 2 []
        s3 = State 3 []
        pa = Pred 0 [a]
        pb = Pred 0 [b]
        pc = Pred 0 [c]
        pd = Pred 0 [d]
        myStates = NL.fromList [s0, s1, s2, s3]
        myTrans :: TransRel
        myTrans = NL.fromList [(s0, s1),
                               (s1, s2),
                               (s2, s0),
                               (s0, s3),
                               (s3, s3)]
        mySat :: SatRel
        mySat = NL.fromList [(s0, pa),
                             (s1, pb),
                             (s2, pc),
                             (s3, pd)]
        myKripkeStructure = (myStates, myTrans, mySat)
        vx = Var 0 []
        vy = Var 1 []
        -- is a state with Pred a reachable?
        myFormula = parser "mu v0 . p0_0 | <>v0"
        myFormulaExpected = Mu vx (Disjunction (Predicate pa) (Diamond (Variable vx)))
        -- is a state with Pred d reachable?
        myFormula2 = parser "mu v1 . p0_3 | <>v1"
        myFormula2Expected = Mu vy (Disjunction (Predicate pd) (Diamond (Variable vy)))
        -- not Pred a and not Pred b
        myFormula3 = parser "~(p0_0 | p0_1)"
        myFormula3Expected = Negation (Disjunction (Predicate pa) (Predicate pb))
    in do
        print $ check myKripkeStructure myFormula   -- [a, b, c]
        print $ myFormula == myFormulaExpected
        print $ check myKripkeStructure myFormula2  -- [a, b, c, d]
        print $ myFormula2 == myFormula2Expected
        print $ check myKripkeStructure myFormula3  -- [c, d]
        print $ myFormula3 == myFormula3Expected