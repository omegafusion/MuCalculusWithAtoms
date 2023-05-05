import Prelude (($), print, (==))
import qualified Prelude as P

import qualified NLambda as NL

import SyntaxUtils ( Pred (..) )
import CTLSyntax ( Formula (..))

import Parser (parser)

import ModelCheckerAtoms (check)
import ModelCheckerUtils (State (..))


main :: P.IO()
main = let
           states = NL.fromList [State 0 [], State 1 [], State 2 []]
           transRel = NL.fromList [(State 0 [], State 1 []), (State 1 [], State 2 [])]
           satRel = NL.fromList [(State 0 [], Pred 0 []),
                                 (State 1 [], Pred 1 []),
                                 (State 2 [], Pred 2 []),
                                 (State 0 [], Pred 3 []),
                                 (State 1 [], Pred 3 []),
                                 (State 2 [], Pred 4 [])]
           ks = (states, transRel, satRel)
           f = parser "C [ E (p0 U p2) ]"
           fExpected = P.Right (ExistsUntil (Predicate (Pred 0 [])) (Predicate (Pred 2 [])))
           g = parser "C [ E (p3 U p4) ]"
           gExpected = P.Right (ExistsUntil (Predicate (Pred 3 [])) (Predicate (Pred 4 [])))
       in do print $ f == fExpected;
             print $ check ks f;
             print $ g == gExpected;
             print $ check ks gExpected;