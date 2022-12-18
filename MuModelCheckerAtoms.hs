module MuModelCheckerAtoms (
    State (..),
    TransRel,
    SatRel,
    KripkeModel,
    Interpretation,
    check
) where


import Prelude hiding (filter, map, not, and, or, sum)
import qualified Prelude as P

import NLambda (Atom, Nominal, atom, eq, (/\), fromBool, variant, mapVariables, foldVariables)
import qualified NLambda as NL

import qualified Data.Set as Set

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Syntax (
    Formula (..),
    Pred (..),
    Var (..)) 

import Parser (parser) 

data State = State Int [Atom] deriving (Show, Eq, Ord) -- TODO: Make into a set with atoms

instance Nominal State where
    eq (State i as) (State i' as') = i `eq` i' /\ as `eq` as'
    variants = variant
    mapVariables mvf (State i as) = State i (mapVariables mvf as)
    foldVariables fvf acc (State i as) = foldVariables fvf acc as


type TransRel = NL.Set (State, State) -- TODO: Convert to NLambda set

type SatRel = NL.Set (State, Pred) -- TODO: Convert to NLambda set

type KripkeModel = (NL.Set State, TransRel, SatRel) -- TODO: Convert to NLambda set
-- A Kripke model is a triple consisting of a state set, a transition relation and a satisfaction relation

type Interpretation = Map Var (NL.Set State) -- TODO: Convert to NLambda set
-- An interpretation is a mapping from the variables to the set of states



check :: KripkeModel -> Formula -> NL.Set State
check model formula =
    let (states, trans, sat) = model
        check' :: Formula -> Interpretation -> NL.Set State
        check' formula interpretation =
            case formula of Predicate p -> NL.filter (\x -> (x, p) `NL.member` sat) states
                            Variable v -> interpretation ! v
                            Disjunction p q ->
                                let s = check' p interpretation
                                    t = check' q interpretation
                                in s `NL.union` t
                            IndexedDisjunction s ->
                                NL.sum (NL.map (\(a, p) -> check' p interpretation) s)
                            Negation p ->
                                let s = check' p interpretation
                                in states `NL.difference` s
                            Diamond p ->
                                let s = check' p interpretation
                                    canReach x = NL.not $ NL.isEmpty $ NL.filter (\y -> (x, y) `NL.member` trans) s
                                -- s is the states that satisfy p
                                -- we want the states with AT LEAST ONE successor in s
                                in NL.filter canReach states
                            Mu x p ->
                                -- we need to do a fixpoint computation. start with x = {}
                                -- then do x = [[p]] until x isn't changed
                                let initialInterpretation = Map.insert x NL.empty interpretation
                                    computeFixpoint s currentInterpretation =
                                        let t = check' p currentInterpretation in
                                            if s == t then t
                                            else computeFixpoint t (Map.insert x t currentInterpretation)
                                in computeFixpoint NL.empty initialInterpretation
    in check' formula Map.empty
