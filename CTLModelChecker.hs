module CTLModelChecker (
    State (..),
    check
) where

import Prelude hiding (True, False, filter, map, not, or, sum, toList)
import qualified Prelude as P

import qualified Data.Set as Set

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Foldable (toList)
import Data.Maybe (listToMaybe)

import CTLSyntax (
    Formula (..),
    Pred (..))

newtype State = State Int deriving (Show, Eq, Ord)

type TransRel = Set.Set (State, State)

type SatRel = Set.Set (State, Pred)

type KripkeModel = (Set.Set State, TransRel, SatRel)
-- A Kripke model is a triple consisting of a state set, a transition relation and a satisfaction relation


post :: TransRel -> State -> Set.Set State
post tr s = 
    let transitions = Set.filter (\p -> fst p == s) tr
    in Set.map snd transitions


fix :: Eq a => (a -> a) -> a -> a
fix f v = if f v == v then v else fix f (f v)


check :: KripkeModel -> Formula -> Set.Set State
check model formula = 
    let (states, trans, sat) = model
        checkExistsNext :: Set.Set State -> Set.Set State
        checkExistsNext s = 
            let test x = P.not $ Set.null (post trans x `Set.intersection` s)
            in Set.filter test states
        checkExistsUntil :: Set.Set State -> Set.Set State -> Set.Set State
        checkExistsUntil s t = 
            let f v = t `Set.union` (s `Set.intersection` checkExistsNext v) -- the smallest fixed point of this function 
            in fix f Set.empty
        checkExistsGlobally :: Set.Set State -> Set.Set State
        checkExistsGlobally s =
            let f v = s `Set.intersection` checkExistsNext v -- the largest fixed point of this function
            in fix f states
        check' :: Formula -> Set.Set State
        check' sf = case sf of
            True -> states
            Predicate p -> Set.filter (\x -> (x, p) `elem` sat) states
            Conjunction p q ->
                let s = check' p
                    t = check' q
                in s `Set.intersection` t 
            Negation p -> 
                let s = check' p
                in states `Set.difference` s
            ExistsNext p ->
                let s = check' p
                in checkExistsNext s
            ExistsUntil p q ->
                let s = check' p
                    t = check' q
                in checkExistsUntil s t
            ExistsGlobally p ->
                let s = check' p 
                in checkExistsGlobally s
    in check' formula
