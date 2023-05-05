module MuModelCheckerAtoms (
    Env,
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


type Env = NL.Set (Var, NL.Set State)
(!) :: Env -> Var -> NL.Set State
r ! v = NL.sum $ NL.map P.snd $ NL.filter (\(a, b) -> a `eq` v) r

-- LEFT-BIASED union of two environments
union :: Env -> Env -> Env
map1 `union` map2 =
    let keys1 = NL.map P.fst map1
        keys2 = NL.map P.fst map2
        keys2include = NL.filter (`NL.notMember` keys1) keys2
        map2include = NL.filter ((`NL.member` keys2include) . P.fst) map2
    in map1 `NL.union` map2include

insert :: Var -> NL.Set State -> Env -> Env
insert v s r =
    let r' = NL.singleton (v, s)
    in union r' r


check :: KripkeModel -> Formula -> NL.Set State
check model formula =
    let (states, trans, sat) = model
        check' :: Formula -> Env -> NL.Set State
        check' formula env = case formula of
            Predicate p -> NL.filter (\x -> (x, p) `NL.member` sat) states
            Variable v -> env ! v
            Disjunction p q ->
                let s = check' p env
                    t = check' q env
                in s `NL.union` t
            IndexedDisjunction (bs, s) ->
                NL.sum (NL.map (\(a, p) -> check' p env) s)
            Negation p ->
                let s = check' p env
                in states `NL.difference` s
            Diamond p ->
                let s = check' p env
                    canReach x = NL.not $ NL.isEmpty $ NL.filter (\y -> (x, y) `NL.member` trans) s
                -- s is the states that satisfy p. we want the states with AT LEAST ONE successor in s
                in NL.filter canReach states
            Mu v (bs, s) ->
                -- we need to do a fixpoint computation. start with x = {}
                -- then do x = [[p]] until x isn't changed
                let x = label v
                    vector = NL.map (first $ \as -> Var x as) s
                    initialEnv = NL.map (second (const NL.empty)) vector `union` env         -- Initially variables paired with empty sets
                    extendEnv curr =
                        let new :: Env
                            new = NL.map (\(x, p) -> (x, check' p curr)) vector `union` curr in     -- For a variable and formula set, check the 
                            if new == curr then curr
                            else extendEnv new
                    extended = extendEnv initialEnv
                in extended ! v
    in check' formula NL.empty
