import Prelude hiding (filter, map, not, or, sum)
import qualified Prelude as P

import NLambda (Atom, atom)
import qualified NLambda as NL

import qualified Data.Set as Set

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Syntax (
    Formula (..),
    Pred (..),
    Var (..)) 

import Parser (parser) 

newtype State = State Atom deriving (Show, Eq, Ord) -- TODO: Make into a set with atoms


type TransRel = Set.Set (State, State) -- TODO: Convert to NLambda set

type SatRel = Set.Set (State, Pred) -- TODO: Convert to NLambda set

type KripkeModel = (Set.Set State, TransRel, SatRel) -- TODO: Convert to NLambda set
-- A Kripke model is a triple consisting of a state set, a transition relation and a satisfaction relation

type Interpretation = Map Var (Set.Set State) -- TODO: Convert to NLambda set
-- An interpretation is a mapping from the variables to the set of states



check :: KripkeModel -> Formula -> Set.Set State
check model formula =
    let (states, trans, sat) = model
        check' :: Formula -> Interpretation -> Set.Set State
        check' formula interpretation =
            case formula of Predicate p -> Set.filter (\x -> (x, p) `elem` sat) states
                            Variable v -> interpretation ! v
                            Disjunction p q ->
                                let s = check' p interpretation
                                    t = check' q interpretation
                                in s `Set.union` t
                            Negation p ->
                                let s = check' p interpretation
                                in states `Set.difference` s
                            Diamond p ->
                                let s = check' p interpretation
                                    canReach x = P.not $ Set.null $ Set.filter (\y -> (x, y) `elem` trans) s
                                -- s is the states that satisfy p
                                -- we want the states with AT LEAST ONE successor in s
                                in Set.filter canReach states
                            Mu x p ->
                                -- we need to do a fixpoint computation. start with x = {}
                                -- then do x = [[p]] until x isn't changed
                                let initialInterpretation = Map.insert x Set.empty interpretation
                                    computeFixpoint s currentInterpretation =
                                        let t = check' p currentInterpretation in
                                            if s == t then t
                                            else computeFixpoint t (Map.insert x t currentInterpretation)
                                in computeFixpoint Set.empty initialInterpretation
    in check' formula Map.empty


main :: IO ()
main = 
    let myStates :: Set.Set State
        myStates = Set.fromList $ P.map State [a, b, c, d]
        a = atom "a"
        b = atom "b"
        c = atom "c"
        d = atom "d"
        myTrans :: TransRel
        myTrans = Set.fromList [(State a, State b),
                            (State b, State c),
                            (State c, State a),
                            (State a, State d),
                            (State d, State d)]
        mySat :: SatRel
        mySat = Set.fromList [(State a, Pred a),
                          (State b, Pred b),
                          (State c, Pred c),
                          (State d, Pred d)]
        myKripkeStructure = (myStates, myTrans, mySat)
        pa = Pred a
        pb = Pred b
        pc = Pred c
        pd = Pred d
        vx = Var "x" []
        vy = Var "y" []
        -- is a state with Pred a reachable?
        myFormula = parser "mu vx . pa | <>vx"
        myFormulaExpected = Mu vx (Disjunction (Predicate pa) (Diamond (Variable vx)))
        -- is a state with Pred d reachable?
        myFormula2 = parser "mu vy . pd | <>vy"
        myFormula2Expected = Mu vy (Disjunction (Predicate pd) (Diamond (Variable vy)))
        -- not Pred a and not Pred b
        myFormula3 = parser "~(pa | pb)"
        myFormula3Expected = Negation (Disjunction (Predicate pa) (Predicate pb))
    in do
        print $ check myKripkeStructure myFormula
        print $ myFormula == myFormulaExpected
        print $ check myKripkeStructure myFormula2
        print $ myFormula2 == myFormula2Expected
        print $ check myKripkeStructure myFormula3
        print $ myFormula3 == myFormula3Expected
