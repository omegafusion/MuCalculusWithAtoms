module CTLModelCheckerAtoms (
    State (..),
    check
) where

import Prelude hiding (True, False, filter, map, not, or, sum, toList)
import qualified Prelude as P

import NLambda (Atom, Nominal, (/\), eq, contains, variants, variant, mapVariables, foldVariables)
import qualified NLambda as NL
--import qualified Data.Set as Set

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Foldable (toList)
import Data.Maybe (listToMaybe)

import CTLSyntax (
    Formula (..),
    Pred (..))

data State = State Int [Atom] deriving (Show, Eq, Ord)

instance Nominal State where
    eq (State n as) (State n' as') = eq n n' /\ eq as as'
    variants = variant
    mapVariables mvf (State n as) = State n (mapVariables mvf as)
    foldVariables fvf acc (State n as) = foldVariables fvf acc as


type TransRel = NL.Set (State, State)

type SatRel = NL.Set (State, Pred)

type KripkeModel = (NL.Set State, TransRel, SatRel)
-- A Kripke model is a triple consisting of a state set, a transition relation and a satisfaction relation


post :: TransRel -> State -> NL.Set State
post tr s = 
    let transitions = NL.filter (\p -> fst p `eq` s) tr
    in NL.map snd transitions


fix :: Eq a => (a -> a) -> a -> a
fix f v = if f v == v then v else fix f (f v)


check :: KripkeModel -> Formula -> NL.Set State
check model formula = 
    let (states, trans, sat) = model
        checkExistsNext :: NL.Set State -> NL.Set State
        checkExistsNext s = 
            let test x = NL.not $ NL.isEmpty (post trans x `NL.intersection` s)
            in NL.filter test states
        checkExistsUntil :: NL.Set State -> NL.Set State -> NL.Set State
        checkExistsUntil s t = 
            let f v = t `NL.union` (s `NL.intersection` checkExistsNext v) -- the smallest fixed point of this function 
            in fix f NL.empty
        checkExistsGlobally :: NL.Set State -> NL.Set State
        checkExistsGlobally s =
            let f v = s `NL.intersection` checkExistsNext v -- the largest fixed point of this function
            in fix f states
        check' :: Formula -> NL.Set State
        check' sf = case sf of
            True -> states
            Predicate p -> NL.filter (\x -> sat `contains` (x, p)) states
            Disjunction p q ->
                let s = check' p
                    t = check' q
                in s `NL.union` t 
            IndexedDisjunction s ->
                NL.sum (NL.map (\(a, p) -> check' p) s)
            Negation p -> 
                let s = check' p
                in states `NL.difference` s
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
