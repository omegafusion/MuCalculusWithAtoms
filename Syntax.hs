module Syntax
    (Formula (..),
     Pred (..),
     Var (..),
     substitute) where

import Prelude ((==), (.), Show, Eq, Ord, Bool, undefined, show, compare)
import qualified Prelude as P

import NLambda ((/\), Atom, Set, Nominal, eq, variant, mapVariables, foldVariables)
import qualified NLambda as NL


--currently, P = A * {0}
--           X = A * {1}
--which are equivariant sets
--TODO: permit arbritrary sets with atoms??

newtype Pred = Pred Atom deriving (Show, Eq, Ord)

newtype Var = Var Atom deriving (Show, Eq, Ord)

data Formula
      = Predicate Pred
      | Boolean Bool
      | Variable Var
      | Disjunction Formula Formula
      | Negation Formula
      | Diamond Formula
      | Mu Var Formula
      deriving (Show, Eq, Ord) -- Syntactic equality only


-- Formulas are nominal types since they contain atoms.
-- This only makes sense if Pred and Var are also nominal types. 

instance Nominal Pred where
      eq (Pred a) (Pred b) = eq a b
      variants = variant
      mapVariables f (Pred a) = Pred (mapVariables f a)
      foldVariables f acc (Pred a) = foldVariables f acc a

instance Nominal Var where
      eq (Var x) (Var y) = eq x y   -- TODO: syntactic or semantic equivalence?
      variants = variant
      mapVariables f (Var x) = Var (mapVariables f x)
      foldVariables f acc (Var a) = foldVariables f acc a

instance Nominal Formula where
      
      -- Two formulas are equivalent if they are semantically equal. -- TODO: syntactic or semantic equivalence?
      eq (Predicate a) (Predicate b) = eq a b
      eq (Variable x) (Variable y) = eq x y
      eq (Disjunction p q) (Disjunction r s) = eq p r /\ eq q s
      eq (Negation p) (Negation q) = eq p q
      eq (Diamond p) (Diamond q) = eq p q
      eq (Mu x p) (Mu y q) = eq x y /\ eq p q   -- TODO: Do we really need to insist on the same variable?
      eq _ _ = NL.false

      variants = variant

      mapVariables f formula = case formula of
            Predicate a -> Predicate (mapVariables f a)
            Variable x -> Variable (mapVariables f x)
            Disjunction p q -> Disjunction (mapVariables f p) (mapVariables f q)
            Negation p -> Negation (mapVariables f p)
            Diamond p -> Diamond (mapVariables f p)
            Mu x p -> Mu (mapVariables f x) (mapVariables f p)

      foldVariables f acc formula = case formula of
            Predicate a -> foldVariables f acc a
            Variable x -> foldVariables f acc x 
            Disjunction p q -> foldVariables f (foldVariables f acc p) q
            Negation p -> foldVariables f acc p
            Diamond p -> foldVariables f acc p
            Mu x p -> foldVariables f (foldVariables f acc x) p


-- TODO: Fix dual and substitute. Formula needs to be a NominalType it seems
dual :: Formula -> Formula
dual (Disjunction p q) = Negation (Disjunction (Negation p) (Negation q))
dual (Diamond p) = Negation (Diamond (Negation p))
dual (Mu x p) = Negation (Mu x (Negation (substitute x (Negation (Variable x)) p)))


substitute :: Var -> Formula -> Formula -> Formula
substitute x t =
    let sub (Variable y) =
              if x==y then t else Variable y
        sub (Predicate a) = Predicate a
        sub (Negation p) = Negation (sub p)
        sub (Disjunction p q) = Disjunction (sub p) (sub q)
        sub (Diamond p) = Diamond (sub p)
        sub (Mu y p) = Mu y (if x==y then p else sub p)
            -- if the variable we're substituting is bound,
            -- it's not really the same variable  
    in sub