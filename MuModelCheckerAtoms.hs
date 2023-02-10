module MuModelCheckerAtoms (
    Interpretation,
    check
) where


import Prelude hiding (filter, map, not, and, or, sum)
import qualified Prelude as P

import NLambda (Atom, Nominal, atom, eq, (/\), fromBool, variant, mapVariables, foldVariables)
import qualified NLambda as NL

import Data.Bifunctor (first, second)

import MuSyntax (
    Formula (..),
    Pred (..),
    Var (..),
    label) 

import ModelCheckerUtils ( State (..), TransRel, SatRel, KripkeModel )



--type Interpretation = Map Var (NL.Set State)
-- An interpretation is a mapping from the variables to the set of states

type Interpretation = NL.Set (Var, NL.Set State)
(!) :: Interpretation -> Var -> NL.Set State
r ! v = NL.sum $ NL.map P.snd $ NL.filter (\(a, b) -> a `eq` v) r

-- LEFT-BIASED union of two interpretations
union :: Interpretation -> Interpretation -> Interpretation
map1 `union` map2 =
    let keys1 = NL.map P.fst map1
        keys2 = NL.map P.fst map2
        keys2include = NL.filter (`NL.notMember` keys1) keys2
        map2include = NL.filter ((`NL.member` keys2include) . P.fst) map2
    in map1 `NL.union` map2include

insert :: Var -> NL.Set State -> Interpretation -> Interpretation
insert v s r =
    let r' = NL.singleton (v, s)
    in union r' r


check :: KripkeModel -> Formula -> NL.Set State
check model formula =
    let (states, trans, sat) = model
        check' :: Formula -> Interpretation -> NL.Set State
        check' formula interpretation = case formula of
            Predicate p -> NL.filter (\x -> (x, p) `NL.member` sat) states
            Variable v -> interpretation ! v
            Disjunction p q ->
                let s = check' p interpretation
                    t = check' q interpretation
                in s `NL.union` t
            IndexedDisjunction (bs, s) ->
                NL.sum (NL.map (\(a, p) -> check' p interpretation) s)
            Negation p ->
                let s = check' p interpretation
                in states `NL.difference` s
            Diamond p ->
                let s = check' p interpretation
                    canReach x = NL.not $ NL.isEmpty $ NL.filter (\y -> (x, y) `NL.member` trans) s
                -- s is the states that satisfy p. we want the states with AT LEAST ONE successor in s
                in NL.filter canReach states
            Mu v (bs, s) ->
                -- we need to do a fixpoint computation. start with x = {}
                -- then do x = [[p]] until x isn't changed
                let x = label v
                    vector = NL.map (first $ \as -> Var x as) s
                    initialInterpretation = NL.map (second (const NL.empty)) vector `union` interpretation         -- Initially variables paired with empty sets
                    extendInterpretation curr =
                        let new :: Interpretation
                            new = NL.map (\(x, p) -> (x, check' p curr)) vector `union` curr in     -- For a variable and formula set, check the 
                            if new == curr then curr
                            else extendInterpretation new
                    extended = extendInterpretation initialInterpretation
                in extended ! v
    in check' formula NL.empty
