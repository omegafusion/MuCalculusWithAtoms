module Syntax
    (Formula (..),
     Pred,
     Var,
     substitute) where

--newtype Pred = Pred Int
type Pred = Int

type Var = Int

data Formula
      = Predicate Pred
      | Variable Var
      | Disjunction Formula Formula
      | Negation Formula
      | Diamond Formula
      | Mu Var Formula
      deriving Show


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
        sub (Mu y p) =
              -- if the variable we're substituting is bound,
              -- it's not really the same variable
              if x==y then p else sub p
    in sub