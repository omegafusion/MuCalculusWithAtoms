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


extend :: Interpretation -> Var -> Set State -> Interpretation
extend p x s y =
    -- extend p by mapping x to s
    -- the extended interpretation takes an argument y
    if y == x then s
    else p y


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
                            Mu x p ->
                                -- we need to do a fixpoint computation
                                -- start with x = empty
                                -- then do x = [[p]]
                                -- until x isn't changed
                                let initialInterpretation = extend emptyInterpretation x empty
                                    computeFixpoint s currentInterpretation =
                                        let t = check' p currentInterpretation in
                                            if s == t then t
                                            else computeFixpoint t (extend currentInterpretation x t)
                                in computeFixpoint empty initialInterpretation
    in check' formula emptyInterpretation


main = 
    let myStates = fromList [0, 1, 2, 7]
        myPredicates = fromList [3, 4, 5, 8]
        myTrans s =
            if s == 0 then fromList [1, 7]
            else if s < 3 then singleton ((s+1) `mod` 3)
            else singleton s
        mySat s p =
            if s < 3 then s+3 == p
            else (p == 8)
        myKripkeStructure = (myStates, myTrans, mySat)
        myFormula = Mu 6 (Disjunction (Predicate 3) (Diamond (Variable 6)))
        myFormula2 = Mu 9 (Disjunction (Predicate 8) (Diamond (Variable 9)))
    in do {
        print $ check myKripkeStructure myFormula;
        print $ check myKripkeStructure myFormula2;
    }
