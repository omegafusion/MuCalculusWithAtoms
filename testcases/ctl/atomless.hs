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
        s i = State i []
        p i = Pred i []
        states = NL.fromList $ map s [0..6]
        transRel = NL.fromList $ [
            (s 0, s 1),
            (s 0, s 2),
            (s 1, s 3),
            (s 1, s 4),
            (s 2, s 5),
            (s 2, s 6)] ++ map (\n -> (s n, s n)) [3..6]
        satRel = NL.fromList $ map (\n -> (s n, p n)) [0..6] ++ [(s 3, p 7), (s 4, p 7)] ++ [(s 0, p 8), (s 2, p 8), (s 5, p 8)]
        kripkeStructure = (states, transRel, satRel)
        -- reachability - p2 is reached on some path
        formula1 = parser "C [ E F p4 ]"
        formula5 = parser "C [ E G p4 ]" -- should only hold in the last state of this path
        -- invariance - p6 holds forever
        formula2 = parser "C [ A G p6 ]"
        -- inevitability - p7 always holds eventually
        formula3 = parser "C [ A F p7 ]"
        -- there is a path where p8 holds forever
        formula4 = parser "C [ E G p8 ]"
    in do
        print $ check kripkeStructure formula1 `NL.eq` (NL.fromList $ map (\i -> State i []) [0, 1, 4]); -- expected: [0, 1, 4]
        print $ check kripkeStructure formula5 `NL.eq` NL.singleton (State 4 []);                        -- expected: [4]
        print $ check kripkeStructure formula2 `NL.eq` NL.singleton (State 6 []);                        -- expected: [6]
        print $ check kripkeStructure formula3 `NL.eq` (NL.fromList $ map (\i -> State i []) [1, 3, 4]); -- expected: [1, 3, 4]
        print $ check kripkeStructure formula4 `NL.eq` (NL.fromList $ map (\i -> State i []) [0, 2, 5]); -- expected: [0, 2, 5]