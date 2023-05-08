import Prelude hiding (parser)
import qualified NLambda as NL

import SyntaxUtils ( Pred (..) )
import CTLSyntax ( Formula (..))

import Parser (parser)

import ModelCheckerUtils (State (..))
import ModelCheckerAtoms (check)


main :: IO ()
main = 
    let 
        states = NL.singleton (State 0 []) `NL.union` (NL.map (\a -> State 1 [a]) NL.atoms) `NL.union` (NL.map (\as -> State 2 as) $ NL.replicateAtoms 2)
        transRel = (NL.map (\a -> (State 0 [], State 1 [a])) NL.atoms) `NL.union` (NL.map (\[a,b] -> (State 1 [a], State 2 [a,b])) $ NL.replicateAtoms 2) `NL.union` (NL.map (\[a,b] -> (State 2 [a,b], State 2 [a,b])) $ NL.replicateAtoms 2)
        satRel = NL.map (\(State i as) -> (State i as, Pred i as)) states
        kripkeStructure = (states, transRel, satRel)

        formula1 = parser "C [ |_a . |_b . E G p2_a,b ]"
        formula2 = parser "C [ |_a . |_b . E F p2_a,b ]"
        formula3 = parser "C [ |_a . |_b . A F p2_a,b ]"
        formula4 = parser "C [ A F (|_a . |_b . p2_a,b) ]"
        formula5 = parser "C [ &_a . E F (|_b . p2_a,b ) ]"
        formula6 = parser "C [ &_a . |_b . E F p2_a,b ]"
        formula7 = parser "C [ |_b . &_a . E F p2_a,b ]"
    in do
        print $ check kripkeStructure formula1 `NL.eq` NL.map (\as -> State 2 as) (NL.replicateAtoms 2); -- [state2_a,b for all a,b]
        print $ check kripkeStructure formula2 `NL.eq` states; -- [all states]
        print $ check kripkeStructure formula3 `NL.eq` NL.map (\as -> State 2 as) (NL.replicateAtoms 2); -- [only state2_a,b for all a,b]
        print $ check kripkeStructure formula4 `NL.eq` states; -- [all states]
        print $ check kripkeStructure formula5 `NL.eq` NL.singleton (State 0 []); -- [only the initial state, s0]
        print $ check kripkeStructure formula6 `NL.eq` NL.singleton (State 0 []); -- [only the initial state, s0]
        print $ check kripkeStructure formula7 `NL.eq` NL.singleton (State 0 []); -- [only the initial state, s0]