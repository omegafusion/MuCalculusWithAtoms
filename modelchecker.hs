import Data.Set


type State = Int

type Pred = Int

type Var = Int

type TransRel = State -> Set State

type SatRel = State -> Pred -> Bool

type KripkeModel = (Set State, TransRel, SatRel)


-- A Kripke model is a triple consisting of a state set, a transition relation and a satisfaction relation

-- Our set of propositions will just be the integers
-- As will our set of variables

data Formula =
    Predicate Pred
    | Variable Var
    | Disjunction Formula Formula
    | Negation Formula
    | Diamond Formula
    | Mu Var Formula

type Interpretation = Var -> Set State
-- An interpretation is a (partial) function from the variables to the set of states

emptyInterpretation :: Interpretation
emptyInterpretation _ = error "free variable"


check :: KripkeModel -> Formula -> Set State
check model formula =
    let (states, trans, sat) = model
        check' :: Formula -> Interpretation -> Set State
        check' formula interpretation =
            case formula of Predicate p -> Data.Set.filter (`sat` p) states
                            Variable v -> interpretation v
                            Disjunction p q ->
                                let s = check' p interpretation
                                    t = check' q interpretation
                                in s `union` t  -- TODO
                            Negation p ->
                                let s = check' p interpretation
                                in states `difference` s  -- TODO
                            Diamond p ->
                                let s = check' p interpretation
                                -- s is the states that satisfy p
                                -- we want the states with AT LEAST ONE successor in s
                                in Data.Set.filter (\x -> not (trans x `disjoint` s)) states
    in check' formula emptyInterpretation


main = 
    let myStates = fromList [0, 1, 2]
        myPredicates = fromList [3, 4, 5]
        myTrans s = singleton ((s+1) `mod` 3) 
        mySat s p = (s+3 == p)

        myKripkeStructure = (myStates, myTrans, mySat)

        myFormula = Diamond (Disjunction (Predicate 3) (Predicate 4))
    in do {
        print $ check myKripkeStructure myFormula;
    }