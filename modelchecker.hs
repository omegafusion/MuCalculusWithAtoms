import Prelude hiding (filter, map, not, or, sum)
import qualified Prelude

import Data.Set (Set, union, fromList, difference)
import qualified Data.Set as Set

import Data.Map (Map, (!))
import qualified Data.Map as Map


newtype State = State Int deriving (Show, Eq, Ord)

newtype Pred = Pred Int deriving (Show, Eq, Ord)

newtype Var = Var Int deriving (Show, Eq, Ord)

type TransRel = Set (State, State)

type SatRel = Set (State, Pred)

type KripkeModel = (Set State, TransRel, SatRel)
-- A Kripke model is a triple consisting of a state set, a transition relation and a satisfaction relation

data Formula =
    Predicate Pred
    | Variable Var
    | Disjunction Formula Formula
    | Negation Formula
    | Diamond Formula
    | Mu Var Formula

type Interpretation = Map Var (Set State)
-- An interpretation is a (partial) function from the variables to the set of states



check :: KripkeModel -> Formula -> Set State
check model formula =
    let (states, trans, sat) = model
        check' :: Formula -> Interpretation -> Set State
        check' formula interpretation =
            case formula of Predicate p -> Set.filter (\x -> (x, p) `elem` sat) states
                            Variable v -> interpretation ! v
                            Disjunction p q ->
                                let s = check' p interpretation
                                    t = check' q interpretation
                                in s `union` t  -- TODO
                            Negation p ->
                                let s = check' p interpretation
                                in states `difference` s  -- TODO
                            Diamond p ->
                                let s = check' p interpretation
                                    canReach x = Prelude.not $ Set.null $ Set.filter (\y -> (x, y) `elem` trans) s
                                -- s is the states that satisfy p
                                -- we want the states with AT LEAST ONE successor in s
                                in Set.filter canReach states
                            Mu x p ->
                                -- we need to do a fixpoint computation
                                -- start with x = {}}
                                -- then do x = [[p]]
                                -- until x isn't changed
                                let initialInterpretation = Map.insert x Set.empty interpretation
                                    computeFixpoint s currentInterpretation =
                                        let t = check' p currentInterpretation in
                                            if s == t then t
                                            else computeFixpoint t (Map.insert x t currentInterpretation)
                                in computeFixpoint Set.empty initialInterpretation
    in check' formula Map.empty


main :: IO ()
main = 
    let myStates :: Set State
        myStates = fromList $ Prelude.map State [0, 1, 2, 7]
        myPredicates :: Set Pred
        myPredicates = fromList $ Prelude.map Pred [3, 4, 5, 8]
        myTrans :: TransRel
        myTrans = fromList [(State 0, State 1),
                            (State 1, State 2),
                            (State 2, State 0),
                            (State 0, State 7),
                            (State 7, State 7)]
        mySat :: SatRel
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
        myFormula = Mu x (Disjunction (Predicate a) (Diamond (Variable x)))
        -- is a state with Pred 8 reachable?
        myFormula2 = Mu y (Disjunction (Predicate b) (Diamond (Variable y)))
        -- not Pred 3 and not Pred 8
        myFormula3 = Negation (Disjunction (Predicate a) (Predicate b))
    in do {
        print $ check myKripkeStructure myFormula;
        print $ check myKripkeStructure myFormula2;
        print $ check myKripkeStructure myFormula3;
    }
