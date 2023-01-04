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
        myStates = fromList $ Prelude.map State [0, 1, 2, 7]
        myPredicates = fromList $ Prelude.map Pred [3, 4, 5, 8]
        myTrans = fromList [(State 0, State 1),
                            (State 1, State 2),
                            (State 2, State 0),
                            (State 0, State 7),
                            (State 7, State 7)]
        mySat = fromList [(State 0, Pred 3),
                          (State 1, Pred 4),
                          (State 2, Pred 5),
                          (State 7, Pred 8)]
        myKripkeStructure = (myStates, myTrans, mySat)
        a = Pred 3
        b = Pred 8
        x = Var 6
        y = Var 9
        -- is a state with Pred 3 reachable?
        myFormula = parser "mu v6 . p3 | <>v6"
        myFormulaExpected = Mu x (Disjunction (Predicate a) (Diamond (Variable x)))
        -- is a state with Pred 8 reachable?
        myFormula2 = parser "mu v9 . p8 | <>v9"
        myFormula2Expected = Mu y (Disjunction (Predicate b) (Diamond (Variable y)))
        -- not Pred 3 and not Pred 8
        myFormula3 = Negation (Disjunction (Predicate a) (Predicate b))
    in do
        print $ check myKripkeStructure myFormula
        print $ myFormula == myFormulaExpected
        print $ check myKripkeStructure myFormula2
        print $ myFormula2 == myFormula2Expected
        print $ check myKripkeStructure myFormula3;
