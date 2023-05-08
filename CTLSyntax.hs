module CTLSyntax (
    Formula (..)
) where

import Prelude (Eq, Ord, Show, Int, Bool)
import qualified Prelude as P

import NLambda (
    (/\),
    Atom,
    Nominal, 
    Set, 
    eq, 
    variants, 
    variant, 
    mapVariables, 
    foldVariables,
    true,
    false )
import qualified NLambda as NL

import SyntaxUtils (Pred (..))


type FormulaSet = Set ([Atom], Formula)

data Formula
    = Predicate Pred
    | Boolean Bool
    | Disjunction Formula Formula
    | IndexedDisjunction FormulaSet
    | Negation Formula
    | ExistsNext Formula
    | ExistsUntil Formula Formula
    | ExistsGlobally Formula
    deriving (Eq, Ord, Show)


instance Nominal Formula where

    formula `eq` formula' = case (formula, formula') of
        (Predicate a, Predicate a') -> a `eq` a'
        (Boolean a, Boolean a') -> a `eq` a'
        (IndexedDisjunction s, IndexedDisjunction s') -> s `eq` s'
        (Disjunction p q, Disjunction p' q') -> p `eq` p' /\ q `eq` q'
        (Negation p, Negation p') -> p `eq` p'
        (ExistsNext p, ExistsNext p') -> p `eq` p'
        (ExistsUntil p q, ExistsUntil p' q') -> p `eq` p' /\ q `eq` q'
        (ExistsGlobally p, ExistsGlobally p') -> p `eq` p'
        (_, _) -> false

    variants = variant

    mapVariables f formula = case formula of
        Predicate a -> Predicate (mapVariables f a)
        Boolean a -> Boolean (mapVariables f a)
        IndexedDisjunction s -> IndexedDisjunction (mapVariables f s)
        Disjunction p q -> Disjunction (mapVariables f p) (mapVariables f q)
        Negation p -> Negation (mapVariables f p)
        ExistsNext p -> ExistsNext (mapVariables f p)
        ExistsUntil p q -> ExistsUntil (mapVariables f p) (mapVariables f q)
        ExistsGlobally p -> ExistsGlobally (mapVariables f p)

    foldVariables f acc formula = case formula of
        Predicate a -> foldVariables f acc a
        Boolean a -> foldVariables f acc a
        IndexedDisjunction s -> foldVariables f acc s
        Disjunction p q -> foldVariables f (foldVariables f acc p) q
        Negation p -> foldVariables f acc p
        ExistsNext p -> foldVariables f acc p
        ExistsUntil p q -> foldVariables f (foldVariables f acc p) q
        ExistsGlobally p -> foldVariables f acc p
