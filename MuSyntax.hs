module MuSyntax
    (Formula (..),
     Pred (..),
     Var (..),
     negateVars,
     freeLabels,
     label) where

import Prelude ((==), (.), (+), (>=), (&&), ($), Show, Eq, Ord, Bool, Int, undefined, show, compare, otherwise, maximum)
import qualified Prelude as P

import NLambda ((/\), Atom, Set, Nominal, eq, variant, mapVariables, foldVariables, atoms)
import qualified NLambda as NL

import SyntaxUtils (Pred)

import Data.Bifunctor (second)
import Data.List ( (++), elem, null, foldr, notElem, delete )


type Label = Int

data Var = Var Label [Atom] deriving (Show, Eq, Ord)

type FormulaSet = ([Label], Set ([Atom], Formula))

data Formula
    = Predicate Pred
    | Boolean Bool
    | Variable Var
    | IndexedDisjunction FormulaSet
    | Disjunction Formula Formula
    | Negation Formula
    | Diamond Formula
    | Mu Var FormulaSet
    deriving (Show, Ord)


instance Eq Formula where   -- Syntactic equality up to alpha conversion
    formula == formula' = case (formula, formula') of
        (Predicate a, Predicate b) -> a == b
        (Boolean a, Boolean b) -> a == b
        (Variable x, Variable y) -> x == y
        (IndexedDisjunction s, IndexedDisjunction s') -> s == s'
        (Disjunction p q, Disjunction p' q') -> p == p' && q == q'
        (Negation p, Negation p') -> p == p'
        (Diamond p, Diamond p') -> p == p' 
        (Mu v (bs, s), Mu v' (bs', s')) ->
            let (Var x as) = v 
                (Var x' as') = v'
                fl = bs ++ bs'
                y = freshLabelFrom (x : x' : fl)
            in as == as' && bs == bs' && NL.map (second (labelswap x y)) s == NL.map (second (labelswap x' y)) s'
        (_, _) -> P.False


instance Nominal Var where
    (Var i as) `eq` (Var i' as') = i `eq` i' /\ as `eq` as'
    variants = variant
    mapVariables f (Var i as) = Var i (mapVariables f as)
    foldVariables f acc (Var i as) = foldVariables f acc as


instance Nominal Formula where
    formula `eq` formula' = case (formula, formula') of
        (Predicate a, Predicate b) -> a `eq` b
        (Boolean a, Boolean b) -> a `eq` b
        (Variable x, Variable y) -> x `eq` y
        (IndexedDisjunction s, IndexedDisjunction s') -> s `eq` s'
        (Disjunction p q, Disjunction p' q') -> p `eq` p' /\ q `eq` q'
        (Negation p, Negation p') -> p `eq` p'
        (Diamond p, Diamond p') -> p `eq` p' 
        (Mu v (bs, s), Mu v' (bs', s')) ->
            let (Var x as) = v 
                (Var x' as') = v'
                fl = bs ++ bs'
                y = freshLabelFrom (x : x' : fl)
            in as `eq` as' /\ bs `eq` bs' /\ NL.map (second (labelswap x y)) s `eq` NL.map (second (labelswap x' y)) s'
        (_, _) -> NL.false
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
                    Mu v (bs, s) -> let x = label v in delete x bs
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
            Predicate a -> Predicate a
            Boolean a -> Boolean a
            Variable v -> Variable (lsvar x y v)
            IndexedDisjunction (bs, s) -> IndexedDisjunction (P.map (ls x y) bs, NL.map (second (labelswap x y)) s)
            Disjunction p q -> Disjunction (labelswap x y p) (labelswap x y q)
            Negation p -> Negation (labelswap x y p)
            Diamond p -> Diamond (labelswap x y p)
            Mu v (bs, s) -> Mu (lsvar x y v) (P.map (ls x y) bs, NL.map (second (labelswap x y)) s)


negateVars :: [Label] -> Formula -> Formula
negateVars xs =
    let sub (Predicate a) =
            Predicate a
        sub (Boolean a) =
            Boolean a
        sub (Variable v) =
            let y = label v in if y `elem` xs then Negation (Variable v) else Variable v
        sub (Negation p) =
            Negation (sub p)
        sub (IndexedDisjunction (bs, s)) =
            IndexedDisjunction (bs, NL.map (second sub) s)
        sub (Disjunction p q) =
            Disjunction (sub p) (sub q)
        sub (Diamond p) =
            Diamond (sub p)
        sub (Mu v (bs, s))
            | y `elem` xs  = Mu v (bs, NL.map (second (negateVars (delete y xs))) s) -- since x does not occur free in p
            | otherwise    = Mu v (bs, NL.map (second sub) s)
            where y = label v
    in sub
