module Syntax
    (Formula (..),
     AtomicFormula (..),
     Pred (..),
     Var (..),
     negation,
     substitute) where


newtype Pred = Pred Int deriving (Show, Eq, Ord)

newtype Var = Var Int deriving (Show, Eq, Ord)

data Formula
      = Positive AtomicFormula
      | Negative AtomicFormula
      | Disjunction Formula Formula
      | Conjunction Formula Formula
      | Diamond Formula
      | Box Formula
      | Mu Var Formula
      | Nu Var Formula
      deriving Show

data AtomicFormula
      = Predicate Pred
      | Variable Var
      deriving Show


negation :: Formula -> Formula
negation (Positive p) = (Negative p)
negation (Negative p) = (Positive p)
negation (Disjunction p q) = Conjunction (negation p) (negation q)
negation (Conjunction p q) = Disjunction (negation p) (negation q)
negation (Diamond p) = Box (negation p)
negation (Box p) = Diamond (negation p)
negation (Mu x p) = Nu x (negation (substitute x nx p))
      where nx = Negative (Variable x)
negation (Nu x p) = Mu x (negation (substitute x nx p))
      where nx = Negative (Variable x)


--dual :: Formula -> Formula
--dual (Disjunction p q) = Negation (Disjunction (Negation p) (Negation q))
--dual (Diamond p) = Negation (Diamond (Negation p))
--dual (Mu x p) = Negation (Mu x (Negation (substitute x (Negation (Variable x)) p)))


substitute :: Var -> Formula -> Formula -> Formula
substitute x t =
    let subAtomic (Predicate a) = Positive (Predicate a)
        subAtomic (Variable y) = if x==y then t else Positive (Variable y)
        sub (Positive a) = subAtomic a
        sub (Negative a) = negation (subAtomic a)
        sub (Disjunction p q) = Disjunction (sub p) (sub q)
        sub (Conjunction p q) = Conjunction (sub p) (sub q)
        sub (Diamond p) = Diamond (sub p)
        sub (Box p) = Box (sub p)
        sub (Mu y p) =
              -- if the variable we're substituting is bound,
              -- it's not really the same variable
              if x==y then p else sub p
        sub (Nu y p) = if x==y then Nu y p else Nu y (sub p)
    in sub