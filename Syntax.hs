module Syntax
    (Formula (..),
     Pred (..),
     Var (..),
     substitute) where

import Prelude ((==), (.), Show, Eq, Ord, Bool, undefined, show, compare)
import qualified Prelude as P

import NLambda (Atom)
import qualified NLambda as NL

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
      deriving (Show, Eq) -- Syntactic equality only



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