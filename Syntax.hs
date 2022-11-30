module Syntax
    (Formula (..),
     Pred (..),
     Var (..),
     substitute) where

import Prelude ((==), (.), Show, Eq, Ord, Bool, String, undefined, show, compare)
import qualified Prelude as P

import NLambda ((/\), Atom, Set, Nominal, eq, variant, mapVariables, foldVariables, atoms)
import qualified NLambda as NL


--currently, P = A * {0}
--           X = A * {1}
--which are equivariant sets
--TODO: permit arbritrary sets with atoms??

newtype Pred = Pred Atom deriving (Show, Eq, Ord)

data Var = Var String [Atom] deriving (Show, Eq, Ord)

data Formula
      = Predicate Pred
      | Boolean Bool
      | Variable Var
      | IndexedDisjunction (Set (Atom, Formula))
      | Disjunction Formula Formula
      | Negation Formula
      | Diamond Formula
      | Mu Var Formula
      deriving (Show, Eq, Ord) -- Syntactic equality only


graphRep :: Nominal a => (Atom -> a) -> Set (Atom, a)
graphRep f = NL.map (\a -> (a, f a)) atoms

-- Formulas are nominal types since they contain atoms.
-- This only makes sense if Pred and Var are also nominal types. 

instance Nominal Pred where
      eq (Pred a) (Pred b) = eq a b
      variants = variant
      mapVariables f (Pred a) = Pred (mapVariables f a)
      foldVariables f acc (Pred a) = foldVariables f acc a

instance Nominal Var where
      eq (Var xlabel xatoms) (Var ylabel yatoms) = 
            NL.fromBool (xlabel == ylabel) /\ eq xatoms yatoms
      variants = variant
      mapVariables f (Var lab as) = Var lab (mapVariables f as)
      foldVariables f acc (Var lab as) = foldVariables f acc as

instance Nominal Formula where

      -- Two formulas are equivalent if they are syntactically equal. -- TODO: syntactic or semantic equivalence?
      eq (Predicate a) (Predicate b) = eq a b
      eq (Variable x) (Variable y) = eq x y
      eq (IndexedDisjunction f) (IndexedDisjunction g) = eq f g
      eq (Disjunction p q) (Disjunction r s) = eq p r /\ eq q s
      eq (Negation p) (Negation q) = eq p q
      eq (Diamond p) (Diamond q) = eq p q
      eq (Mu x p) (Mu y q) = eq x y /\ eq p q   -- TODO: Do we really need to insist on the same variable?
      eq _ _ = NL.false

      variants = variant

      mapVariables f formula = case formula of
            Predicate a -> Predicate (mapVariables f a)
            Variable x -> Variable (mapVariables f x)
            IndexedDisjunction g -> IndexedDisjunction (NL.map (\(a, p) -> (mapVariables f a, mapVariables f p)) g)
            Disjunction p q -> Disjunction (mapVariables f p) (mapVariables f q)
            Negation p -> Negation (mapVariables f p)
            Diamond p -> Diamond (mapVariables f p)
            Mu x p -> Mu (mapVariables f x) (mapVariables f p)

      foldVariables f acc formula = case formula of
            Predicate a -> foldVariables f acc a
            Variable x -> foldVariables f acc x 
            IndexedDisjunction g -> foldVariables f acc g
            Disjunction p q -> foldVariables f (foldVariables f acc p) q
            Negation p -> foldVariables f acc p
            Diamond p -> foldVariables f acc p
            Mu x p -> foldVariables f (foldVariables f acc x) p


-- TODO: Fix dual and substitute. Formula needs to be a NominalType it seems
dual :: Formula -> Formula
dual (Disjunction p q) = Negation (Disjunction (Negation p) (Negation q))
dual (Diamond p) = Negation (Diamond (Negation p))
dual (Mu x p) = Negation (Mu x (Negation (substitute x (Negation (Variable x)) p)))


alphaconvert :: Var -> Var -> Formula -> Formula
alphaconvert x y = substitute x (Variable y) -- TODO: Do something a bit more insightful here maybe


substitute :: Var -> Formula -> Formula -> Formula
substitute x t =
    let sub (Variable y) =
              if x==y then t else Variable y
        sub (Predicate a) = Predicate a
        sub (Negation p) = Negation (sub p)
        sub (IndexedDisjunction g) = IndexedDisjunction (NL.map (\(a, p) -> (a, sub p)) g)
        sub (Disjunction p q) = Disjunction (sub p) (sub q)
        sub (Diamond p) = Diamond (sub p)
        sub (Mu y p) = Mu y (if x==y then p else sub p)
            -- if the variable we're substituting is bound,
            -- it's not really the same variable  
    in sub