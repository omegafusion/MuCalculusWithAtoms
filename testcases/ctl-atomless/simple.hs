import Prelude (($), print)
import qualified Prelude as P

import qualified Data.Set as Set

import CTLSyntax (
    Pred (..),
    Formula (..))

import CTLModelChecker (
    State (..),
    check)


main :: P.IO()
main = let states = Set.fromList [State 0, State 1, State 2]
           transRel = Set.fromList [(State 0, State 1), (State 1, State 2)]
           satRel = Set.fromList [(State 0, Pred 0), (State 1, Pred 1), (State 2, Pred 2), (State 0, Pred 3), (State 1, Pred 3), (State 2, Pred 4)]
           ks = (states, transRel, satRel)
           f = ExistsUntil (Predicate (Pred 0)) (Predicate (Pred 2))
           g = ExistsUntil (Predicate (Pred 3)) (Predicate (Pred 4))
       in do print $ check ks f;
             print $ check ks g;