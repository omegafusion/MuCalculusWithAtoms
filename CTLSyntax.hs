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

    (Predicate a) `eq` (Predicate a') = a `eq` a'
    (Boolean a) `eq` (Boolean a') = a `eq` a'
    (IndexedDisjunction s) `eq` (IndexedDisjunction s') = s `eq` s'
    (Disjunction p q) `eq` (Disjunction p' q') = p `eq` p' /\ q `eq` q'
    (Negation p) `eq` (Negation p') = p `eq` p'
    (ExistsNext p) `eq` (ExistsNext p') = p `eq` p'
    (ExistsUntil p q) `eq` (ExistsUntil p' q') = p `eq` p' /\ q `eq` q'
    (ExistsGlobally p) `eq` (ExistsGlobally p') = p `eq` p'
    _ `eq` _ = false

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
