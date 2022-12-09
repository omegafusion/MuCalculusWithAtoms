import Prelude hiding (filter, map, not, or, sum)
import qualified Prelude as P


import NLambda (Atom, Nominal, atom, constant, eq, variant, mapVariables, foldVariables)
import qualified NLambda as NL


import MuModelCheckerAtoms (
    State (..),
    TransRel,
    SatRel,
    KripkeModel,
    check)


import Syntax (
    Formula (..),
    Pred (..),
    Var (..))


import Parser (parser)


main :: P.IO ()
main = 
    let myStates :: NL.Set State
        myStates = NL.fromList $ P.map State [a, b, c, d]
        a = constant 0
        b = constant 1
        c = constant 2
        d = constant 3
        myTrans :: TransRel
        myTrans = NL.fromList [(State a, State b),
                            (State b, State c),
                            (State c, State a),
                            (State a, State d),
                            (State d, State d)]
        mySat :: SatRel
        mySat = NL.fromList [(State a, Pred a),
                          (State b, Pred b),
                          (State c, Pred c),
                          (State d, Pred d)]
        myKripkeStructure = (myStates, myTrans, mySat)
        pa = Pred a
        pb = Pred b
        pc = Pred c
        pd = Pred d
        vx = Var 0 []
        vy = Var 1 []
        -- is a state with Pred a reachable?
        myFormula = parser "mu v0 . p0 | <>v0"
        myFormulaExpected = Mu vx (Disjunction (Predicate pa) (Diamond (Variable vx)))
        -- is a state with Pred d reachable?
        myFormula2 = parser "mu v1 . p3 | <>v1"
        myFormula2Expected = Mu vy (Disjunction (Predicate pd) (Diamond (Variable vy)))
        -- not Pred a and not Pred b
        myFormula3 = parser "~(p0 | p1)"
        myFormula3Expected = Negation (Disjunction (Predicate pa) (Predicate pb))
    in do
        print $ check myKripkeStructure myFormula   -- [a, b, c]
        print $ myFormula == myFormulaExpected
        print $ check myKripkeStructure myFormula2  -- [a, b, c, d]
        print $ myFormula2 == myFormula2Expected
        print $ check myKripkeStructure myFormula3  -- [c, d]
        print $ myFormula3 == myFormula3Expected
