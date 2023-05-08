import qualified NLambda as NL

import SyntaxUtils ( Pred (..) )

import Parser (parser)

import ModelCheckerAtoms (check)
import ModelCheckerUtils (State (..))

main = 
    let
        formula1raw = "C [ |_a . &_b . A G ~p2_a,b ]"
        formula2raw = "M [ |_a . &_b . nu v0 . ~p2_a,b & []v0 ]"
        --states = NL.map (\a -> State 0 [a]) NL.atoms
        --transRel = NL.map (\[a,b] -> (State 0 [a], State 0 [b])) $ NL.filter (\[a,b] -> a `NL.lt` b) $ NL.replicateAtoms 2
        --satRel = NL.map (\[a,b] -> (State 0 [a], Pred 0 [b])) $ NL.filter (\[a,b] -> a `NL.lt` b) $ NL.replicateAtoms 2
        states = NL.singleton (State 0 []) `NL.union` (NL.map (\a -> State 1 [a]) NL.atoms) `NL.union` (NL.map (\as -> State 2 as) $ NL.replicateAtoms 2)
        transRel = (NL.map (\a -> (State 0 [], State 1 [a])) NL.atoms) `NL.union` (NL.map (\[a,b] -> (State 1 [a], State 2 [a,b])) $ NL.replicateAtoms 2) `NL.union` (NL.map (\[a,b] -> (State 2 [a,b], State 2 [a,b])) $ NL.replicateAtoms 2)
        satRel = NL.map (\(State i as) -> (State i as, Pred i as)) states
        kripkeStructure = (states, transRel, satRel)
    in do
        putStrLn "This is a combined model checker for the mu calculus with atoms, and CTL with atoms."
        putStrLn "consider the property \"there exists an atom a such that for every atom b, no state with p0_a,b is reachable\""
        putStr "We can write this formula in CTL with atoms:   "
        putStrLn formula1raw
        putStr "as well as in the mu calculus with atoms:   "
        putStrLn formula2raw
        putStrLn "consider the following simple Kripke Structure: "
        putStrLn "States: "
        print $ states
        putStrLn "Transition Relation: "
        print $ transRel
        putStrLn "Satisfaction Relation: "
        print $ satRel
        putStrLn "these are the respective results of model checking these formulas:"
        print $ check kripkeStructure (parser formula1raw)
        print $ check kripkeStructure (parser formula2raw)
        putStrLn "which are the same sets of states, and the expected sets of states (i.e. every state apart from the start state)"
        putStrLn "feel free to take a look at the code, and write your own formulas and Kripke structures!"
        putStrLn "there are also plenty of test-cases available to try, and to adapt"