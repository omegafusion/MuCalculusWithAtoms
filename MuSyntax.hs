module MuSyntax
    (Formula (..),
     Pred (..),
     Var (..),
     --substitute,
     negateVars,
     graphRep) where

import Prelude ((==), (.), (+), (>=), (&&), Show, Eq, Ord, Bool, Int, undefined, show, compare, otherwise, maximum)
import qualified Prelude as P

import NLambda ((/\), Atom, Set, Nominal, eq, variant, mapVariables, foldVariables, atoms)
import qualified NLambda as NL

import SyntaxUtils (Pred)

import Data.Bifunctor (second)
import Data.List ( (++), elem, null, foldr, notElem, delete )


--currently, P = A * {0}
--           X = A * {1}
--which are equivariant sets
--TODO: permit arbritrary sets with atoms??

type Label = Int

data Var = Var Label [Atom] deriving (Show, Eq, Ord)

type FormulaSet = ([Label], Set (Atom, Formula))
--type FormulaVector = Set (Var, Formula)

data Formula
      = Predicate Pred
      | Boolean Bool
      | Variable Var
      | IndexedDisjunction FormulaSet
      | Disjunction Formula Formula
      | Negation Formula
      | Diamond Formula
      | Mu Var Formula
      deriving (Show, Ord) -- Syntactic equality only


--instance Eq (Atom -> Formula) where
--    a == b = graphRep a == graphRep b


--instance Ord (Atom -> Formula) where
--    compare a b = compare (graphRep a) (graphRep b)


--instance Show (Atom -> Formula) where
--    show = show . graphRep


instance Eq Formula where
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


graphRep :: Nominal a => (Atom -> a) -> Set (Atom, a)
graphRep f = NL.map (\a -> (a, f a)) atoms

-- Formulas are nominal types since they contain atoms.
-- This only makes sense if Pred and Var are also nominal types. 


instance Nominal Var where
      eq (Var xlabel xatoms) (Var ylabel yatoms) = 
            NL.fromBool (xlabel == ylabel) /\ eq xatoms yatoms
      variants = variant
      mapVariables mvf (Var lab as) = Var lab (mapVariables mvf as)
      foldVariables fvf acc (Var lab as) = foldVariables fvf acc as

--instance Nominal (Atom -> Formula) where
--    eq f g = eq (graphRep f) (graphRep g)
--    variants = variant
--    mapVariables mvf f = \a -> mapVariables mvf (f a)
--    foldVariables fvf acc = foldVariables fvf acc . graphRep


instance Nominal Formula where

      -- Two formulas are equivalent if they are syntactically equal. -- TODO: syntactic or semantic equivalence?
      eq :: Formula -> Formula -> NL.Formula
      eq (Predicate a) (Predicate b) =
        eq a b
      eq (Boolean a) (Boolean b) =
        eq a b
      eq (Variable x) (Variable y) =
        eq x y
      eq (IndexedDisjunction (bs, s)) (IndexedDisjunction (bs', s')) =
        eq bs bs' /\ eq s s'
      eq (Disjunction p q) (Disjunction r s) =
        eq p r /\ eq q s
      eq (Negation p) (Negation q) =
        eq p q
      eq (Diamond p) (Diamond q) =
        eq p q
      eq (Mu x p) (Mu y q) =
        eq x y /\ eq p q
      eq _ _ =
        NL.false

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


{-- TODO: Fix dual and substitute. Formula needs to be a NominalType it seems
dual :: Formula -> Formula
dual (Disjunction p q) = Negation (Disjunction (Negation p) (Negation q))
dual (Diamond p) = Negation (Diamond (Negation p))
dual (Mu x p) = Negation (Mu x (Negation (substitute x (Negation (Variable x)) p)))-}


{-freeVars :: Formula -> [Var]
freeVars formula =
    let fvs xs f = case f of
                    Predicate p -> []
                    Boolean a -> []
                    Variable y -> [y | y `notElem` xs]
                    Disjunction p q -> fvs xs p ++ fvs xs q
                    Negation p -> fvs xs p
                    Diamond p -> fvs xs p
                    Mu x p -> fvs (x:xs) p
    in fvs [] formula -- TODO -}

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


{-freshFrom :: [Var] -> Var
freshFrom vs =
    -- Return an unused variable that INDEXES NO ATOMS
    let newFresh (Var x as) y = if null as && x>=y then x+1 else y
    in Var (foldr newFresh 0 vs) []-}

freshLabelFrom :: [Label] -> Label
freshLabelFrom [] = 0
freshLabelFrom ls = maximum ls + 1


{-nameswap :: Var -> Var -> Formula -> Formula
nameswap x y formula =
    let ns :: Var -> Var -> Var -> Var
        ns x y z
            | z == y    = x
            | z == x    = y
            | otherwise = z
    in case formula of
            Variable z -> Variable (ns x y z)
            Predicate a -> Predicate a
            IndexedDisjunction (bs, s) -> IndexedDisjunction (bs, NL.map (second (nameswap x y)) s)
            Disjunction p q -> Disjunction (nameswap x y p) (nameswap x y q)
            Negation p -> Negation (nameswap x y p)
            Diamond p -> Diamond (nameswap x y p)
            Mu z p -> Mu (ns x y z) (nameswap x y p)-}

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
            IndexedDisjunction (bs, s) -> IndexedDisjunction (P.map (ls x y) bs, NL.map (second (labelswap x y)) s)
            Disjunction p q -> Disjunction (labelswap x y p) (labelswap x y q)
            Negation p -> Negation (labelswap x y p)
            Diamond p -> Diamond (labelswap x y p)
            Mu v p -> Mu (lsvar x y v) (labelswap x y p)


{-substitute :: Var -> Formula -> Formula -> Formula
substitute x t =
    -- might rename bound variables if they appear free in t
    -- find free variables in t
    let fv = freeVars t
        sub (Variable y) =
              if x==y then t else Variable y
        sub (Predicate a) =
            Predicate a
        sub (Negation p) =
            Negation (sub p)
        sub (IndexedDisjunction s) =
            IndexedDisjunction (NL.map (second sub) s)
        sub (Disjunction p q) =
            Disjunction (sub p) (sub q)
        sub (Diamond p) =
            Diamond (sub p)
        sub (Mu y p)
            | x==y         = Mu y p -- x does not occur free in p
            | y `elem` fv  = let z = freshFrom fv in Mu z (sub (substitute y (Variable z) t)) -- TODO
            | otherwise   = Mu y (sub p)
            -- if the variable we're substituting is bound,
            -- it's not really the same variable  
    in sub-}

negateVars :: [Var] -> Formula -> Formula
negateVars xs =
    let sub (Variable y) =
            if y `elem` xs then Negation (Variable y) else Variable y
        sub (Predicate a) =
            Predicate a
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
