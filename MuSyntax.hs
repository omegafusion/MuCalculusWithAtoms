module MuSyntax
    (Formula (..),
     Pred (..),
     Var (..),
     substitute,
     graphRep) where

import Prelude ((==), (.), (+), (>=), (++), (&&), Show, Eq, Ord, Bool, Int, undefined, show, compare, elem, otherwise, null, foldr, notElem)
import qualified Prelude as P

import NLambda ((/\), Atom, Set, Nominal, eq, variant, mapVariables, foldVariables, atoms)
import qualified NLambda as NL

import SyntaxUtils (Pred)

import Data.Bifunctor (second)


--currently, P = A * {0}
--           X = A * {1}
--which are equivariant sets
--TODO: permit arbritrary sets with atoms??

data Var = Var Int [Atom] deriving (Show, Eq, Ord)

--type FormulaVector = Set (Var, Formula)

data Formula
      = Predicate Pred
      | Boolean Bool
      | Variable Var
      | IndexedDisjunction (Set (Atom, Formula))
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
    Mu x p == Mu x' p' =
        let (Var i as) = x 
            (Var i' as') = x'
            fv = freeVars p ++ freeVars p'
            y = freshFrom fv
        in as == as' && nameswap x y p == nameswap x' y p'


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
      eq (IndexedDisjunction f) (IndexedDisjunction g) =
        eq f g
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
            IndexedDisjunction s -> IndexedDisjunction (mapVariables f s)
            Disjunction p q -> Disjunction (mapVariables f p) (mapVariables f q)
            Negation p -> Negation (mapVariables f p)
            Diamond p -> Diamond (mapVariables f p)
            Mu x p -> Mu (mapVariables f x) (mapVariables f p)

      foldVariables f acc formula = case formula of
            Predicate a -> foldVariables f acc a
            Boolean a -> foldVariables f acc a
            Variable x -> foldVariables f acc x 
            IndexedDisjunction s -> foldVariables f acc s
            Disjunction p q -> foldVariables f (foldVariables f acc p) q
            Negation p -> foldVariables f acc p
            Diamond p -> foldVariables f acc p
            Mu x p -> foldVariables f (foldVariables f acc x) p


-- TODO: Fix dual and substitute. Formula needs to be a NominalType it seems
dual :: Formula -> Formula
dual (Disjunction p q) = Negation (Disjunction (Negation p) (Negation q))
dual (Diamond p) = Negation (Diamond (Negation p))
dual (Mu x p) = Negation (Mu x (Negation (substitute x (Negation (Variable x)) p)))


freeVars :: Formula -> [Var]
freeVars formula =
    let fvs xs f = case f of
                    Predicate p -> []
                    Boolean a -> []
                    Variable y -> [y | y `notElem` xs]
                    Disjunction p q -> fvs xs p ++ fvs xs q
                    Negation p -> fvs xs p
                    Diamond p -> fvs xs p
                    Mu x p -> fvs (x:xs) p
    in fvs [] formula -- TODO


freshFrom :: [Var] -> Var
freshFrom vs =
    -- Return an unused variable that INDEXES NO ATOMS
    let newFresh (Var x as) y = if null as && x>=y then x+1 else y
    in Var (foldr newFresh 0 vs) []


nameswap :: Var -> Var -> Formula -> Formula
nameswap x y formula =
    let ns :: Var -> Var -> Var -> Var
        ns x y z
            | z == y    = x
            | z == x    = y
            | otherwise = z
    in case formula of
            Variable z -> Variable (ns x y z)
            Predicate a -> Predicate a
            Disjunction p q -> Disjunction (nameswap x y p) (nameswap x y q)
            Negation p -> Negation (nameswap x y p)
            Diamond p -> Diamond (nameswap x y p)
            Mu z p -> Mu (ns x y z) (nameswap x y p)


substitute :: Var -> Formula -> Formula -> Formula
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
    in sub
