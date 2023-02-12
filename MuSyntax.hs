module MuSyntax
    (Formula (..),
     Pred (..),
     Var (..),
     negateVars,
     freeLabels) where


import Prelude ((==), (.), (+), (>=), (&&), Show, Eq, Ord, Bool, Int, undefined, show, compare, otherwise, maximum)
import qualified Prelude as P

import NLambda ((/\), Atom, Set, Nominal, eq, variant, mapVariables, foldVariables, atoms)
import qualified NLambda as NL

import SyntaxUtils (Pred)

import Data.Bifunctor (second)
import Data.List ( (++), elem, null, foldr, notElem, delete )


--TODO: permit arbritrary sets with atoms??

type Label = Int

data Var = Var Label [Atom] deriving (Show, Eq, Ord)

type FormulaSet = ([Label], Set (Atom, Formula))

data Formula
      = Predicate Pred
      | Boolean Bool
      | Variable Var
      | IndexedDisjunction FormulaSet
      | Disjunction Formula Formula
      | Negation Formula
      | Diamond Formula
      | Mu Var Formula
      deriving (Show, Ord)


instance Eq Formula where   -- Syntactic equality up to alpha conversion
    Predicate a == Predicate b =
        a == b
    Boolean a == Boolean b =
        a == b
    Variable x == Variable y =
        x == y
    IndexedDisjunction s == IndexedDisjunction s' =
        s == s'
    Disjunction p q == Disjunction p' q' =
        p == p' && q == q'
    Negation p == Negation p' =
        p == p'
    Diamond p == Diamond p' =
        p == p'
    Mu v p == Mu v' p' =
        let (Var x as) = v 
            (Var x' as') = v'
            fl = freeLabels p ++ freeLabels p'
            y = freshLabelFrom fl
        in as == as' && labelswap x y p == labelswap x' y p'
    _ == _ =
        P.False

-- Formulas are nominal types since they contain atoms.
-- This only makes sense if Pred and Var are also nominal types. 


instance Nominal Var where
      eq (Var xlabel xatoms) (Var ylabel yatoms) = 
            NL.fromBool (xlabel == ylabel) /\ eq xatoms yatoms
      variants = variant
      mapVariables mvf (Var lab as) = Var lab (mapVariables mvf as)
      foldVariables fvf acc (Var lab as) = foldVariables fvf acc as


instance Nominal Formula where

      eq a b = NL.fromBool (a == b)

      variants = variant

      mapVariables f formula = case formula of
            Predicate a -> Predicate (mapVariables f a)
            Boolean a -> Boolean (mapVariables f a)
            Variable x -> Variable (mapVariables f x)
            IndexedDisjunction (bs, s) -> IndexedDisjunction (bs, mapVariables f s)
            Disjunction p q -> Disjunction (mapVariables f p) (mapVariables f q)
            Negation p -> Negation (mapVariables f p)
            Diamond p -> Diamond (mapVariables f p)
            Mu x p -> Mu (mapVariables f x) (mapVariables f p)

      foldVariables f acc formula = case formula of
            Predicate a -> foldVariables f acc a
            Boolean a -> foldVariables f acc a
            Variable x -> foldVariables f acc x 
            IndexedDisjunction (bs, s) -> foldVariables f acc (bs, s)
            Disjunction p q -> foldVariables f (foldVariables f acc p) q
            Negation p -> foldVariables f acc p
            Diamond p -> foldVariables f acc p
            Mu x p -> foldVariables f (foldVariables f acc x) p


label :: Var -> Label
label (Var x as) = x

changeLabel :: Var -> Label -> Var
changeLabel (Var x as) y = Var y as

freeLabels :: Formula -> [Label]
freeLabels formula =
    let fls xs f = case f of
                    Predicate p -> []
                    Boolean a -> []
                    Variable x -> let i = label x in [i | i `notElem` xs]
                    IndexedDisjunction (bs, s) -> bs
                    Disjunction p q -> fls xs p ++ fls xs q
                    Negation p -> fls xs p
                    Diamond p -> fls xs p
                    Mu x p -> fls (label x : xs) p
    in fls [] formula


freshLabelFrom :: [Label] -> Label
freshLabelFrom [] = 0
freshLabelFrom ls = maximum ls + 1


labelswap :: Label -> Label -> Formula -> Formula
labelswap x y formula =
    let ls :: Label -> Label -> Label -> Label
        ls x y z
            | z == y    = x
            | z == x    = y
            | otherwise = z
        lsvar :: Label -> Label -> Var -> Var
        lsvar x y v = let z = label v in changeLabel v (ls x y z)
    in case formula of
            Variable v -> Variable (lsvar x y v)
            Predicate a -> Predicate a
            Boolean a -> Boolean a
            IndexedDisjunction (bs, s) -> IndexedDisjunction (P.map (ls x y) bs, NL.map (second (labelswap x y)) s)
            Disjunction p q -> Disjunction (labelswap x y p) (labelswap x y q)
            Negation p -> Negation (labelswap x y p)
            Diamond p -> Diamond (labelswap x y p)
            Mu v p -> Mu (lsvar x y v) (labelswap x y p)


negateVars :: [Var] -> Formula -> Formula
negateVars xs =
    let sub (Predicate a) =
            Predicate a
        sub (Boolean a) =
            Boolean a
        sub (Variable y) =
            if y `elem` xs then Negation (Variable y) else Variable y
        sub (Negation p) =
            Negation (sub p)
        sub (IndexedDisjunction (bs, s)) =
            IndexedDisjunction (bs, NL.map (second sub) s)
        sub (Disjunction p q) =
            Disjunction (sub p) (sub q)
        sub (Diamond p) =
            Diamond (sub p)
        sub (Mu y p)
            | y `elem` xs  = Mu y (negateVars (delete y xs) p) -- since x does not occur free in p
            | otherwise    = Mu y (sub p)
    in sub
