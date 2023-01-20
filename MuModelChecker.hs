module MuModelChecker (
    State (..),
    check
) where

import Prelude hiding (filter, map, not, or, sum)
import qualified Prelude as P

import Data.Set (Set, union, fromList, difference)
import qualified Data.Set as Set

import Data.Map (Map, (!))
import qualified Data.Map as Map

import MuSyntax (
    Formula (..),
    Pred (..),
    Var (..),
    getByVar) 

--import Parser (parser) 

newtype State = State Int deriving (Show, Eq, Ord)


type TransRel = Set (State, State)

type SatRel = Set (State, Pred)

type KripkeModel = (Set State, TransRel, SatRel)
-- A Kripke model is a triple consisting of a state set, a transition relation and a satisfaction relation

type Interpretation = Map Var (Set State)
-- An interpretation is a mapping from the variables to the set of states


fix :: Eq a => (a -> a) -> a -> a
fix f v = if f v == v then v else fix f (f v)


check :: KripkeModel -> Formula -> Set State
check model formula =
    let (states, trans, sat) = model
        check' :: Formula -> Interpretation -> Set State
        check' formula interpretation =
            case formula of Predicate p -> Set.filter (\x -> (x, p) `elem` sat) states
                            Variable v -> interpretation ! v
                            Disjunction p q ->
                                let s = check' p interpretation
                                    t = check' q interpretation
                                in s `union` t  -- TODO
                            Negation p ->
                                let s = check' p interpretation
                                in states `difference` s  -- TODO
                            Diamond p ->
                                let s = check' p interpretation
                                    canReach x = P.not $ Set.null $ Set.filter (\y -> (x, y) `elem` trans) s
                                -- s is the states that satisfy p
                                -- we want the states with AT LEAST ONE successor in s
                                in Set.filter canReach states
                            --Mu _ [(x, p)] -> fix (\s -> check' p (Map.insert x s interpretation)) Set.empty
                            Mu i vector ->
                                let (vars, formulas) = unzip vector
                                    initialStateSet = [Set.empty | _ <- vector]
                                    extendStateSet states =
                                        let varStates = zip vars states
                                            r = Map.fromList varStates `Map.union` interpretation
                                        in [check' p r | p <- formulas]
                                in snd (zip vars (fix extendStateSet initialStateSet) !! i)

    in check' formula Map.empty


