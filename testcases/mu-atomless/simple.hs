import MuModelChecker (
    State (..),
    check)

import MuSyntax (
    Pred (..),
    Var (..),
    Formula (..))

import Parser (parser)

import Data.Set (Set, fromList)


main :: IO ()
main = 
    let myStates :: Set State
        myStates = fromList $ Prelude.map State [0, 1, 2, 3]
        myPredicates = fromList $ Prelude.map Pred [0, 1, 2, 3]
        myTrans = fromList [(State 0, State 1),
                            (State 1, State 2),
                            (State 2, State 0),
                            (State 0, State 3),
                            (State 3, State 3)]
        mySat = fromList [(State 0, Pred 0),
                          (State 1, Pred 1),
                          (State 2, Pred 2),
                          (State 3, Pred 3)]
        myKripkeStructure = (myStates, myTrans, mySat)
        a = Pred 0
        b = Pred 3
        x = Var 0
        y = Var 1
        -- is a state with Pred 3 reachable?
        myFormula = parser "mu v0 . p0 | <>v0"
        myFormulaExpected = Mu x [(x, Disjunction (Predicate a) (Diamond (Variable x)))]
        -- is a state with Pred 8 reachable?
        myFormula2 = parser "mu v1 . p3 | <>v1"
        myFormula2Expected = Mu y [(y, Disjunction (Predicate b) (Diamond (Variable y)))]
        -- not Pred 0 and not Pred 3
        myFormula3 = parser "~(p0 | p3)"
        myFormula3Expected = Negation (Disjunction (Predicate a) (Predicate b))

        --myFormula4 = parser "mu v0 . { v0 . p0 | <>v1 , v1 . pl | <>v2 , v2 . p2 | <>v0 }"
        myFormula4 = parser "mu v0 . { v0 . p0 | <>v1 , v1 . p1 | <>v2 , v2 . p2 | <>v0 }"
        myFormula5 = parser "mu v1 . { v0 . p0 | <>v1 , v1 . p1 | <>v2 , v2 . p2 | <>v0 }"
    in do
        print $ myFormula == myFormulaExpected
        print $ check myKripkeStructure myFormulaExpected
        print $ myFormula2 == myFormula2Expected
        print $ check myKripkeStructure myFormula2Expected
        print $ myFormula3 == myFormula3Expected
        print $ check myKripkeStructure myFormula3Expected
        print $ check myKripkeStructure myFormula4
        print $ check myKripkeStructure myFormula5