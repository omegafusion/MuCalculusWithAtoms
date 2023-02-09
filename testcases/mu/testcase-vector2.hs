import qualified NLambda as NL

import Parser (parser)
import ModelCheckerAtoms (check)
import ModelCheckerUtils ( State(..) )
import SyntaxUtils (Pred(..))


main = let phi = parser "M[ &_b . |_a . mu v0_a,b { c,d . p0_c | <>v0_c,d } ]"
           states = NL.map (\[a,b] -> State 0 [a,b]) $ NL.replicateAtoms 2
           transRel = NL.map (\[a,b,c] -> (State 0 [a,b], State 0 [a,c])) $ NL.replicateAtoms 3
           satRel = NL.map (\[a,b] -> (State 0 [a,b], Pred 0 [a])) $ NL.filter (\[a,b] -> b `NL.eq` NL.constant 0) $ NL.replicateAtoms 2
           model = (states, transRel, satRel)
       in print $ check model phi