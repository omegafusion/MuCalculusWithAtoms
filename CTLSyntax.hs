module CTLSyntax (
    Formula (..),
    Pred (..),
    graphRep
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
import GHC.Plugins (mapVarBndr)


data Pred = Pred Int [Atom] deriving (Show, Eq, Ord)

instance Nominal Pred where
    eq (Pred n as) (Pred n' as') = eq n n' /\ eq as as'
    variants = variant
    mapVariables mvf (Pred n as) = Pred n (mapVariables mvf as)
    foldVariables fvf acc (Pred n as) = foldVariables fvf acc as


data Formula
    = Predicate Pred
    | Boolean Bool
    | Disjunction Formula Formula
    | IndexedDisjunction (Set (Atom, Formula))
    | Negation Formula
    | ExistsNext Formula
    | ExistsUntil Formula Formula
    | ExistsGlobally Formula
    deriving (Eq, Ord, Show)


instance Nominal Formula where

      -- Two formulas are equivalent if they are syntactically equal. -- TODO: syntactic or semantic equivalence?
      eq (Predicate a) (Predicate a') =
        eq a a'
      eq (Boolean a) (Boolean a') =
        eq a a'
      eq (IndexedDisjunction f) (IndexedDisjunction f') =
        eq f f'
      eq (Disjunction p q) (Disjunction p' q') =
        eq p p' /\ eq q q'
      eq (Negation p) (Negation p') =
        eq p p'
      eq (ExistsNext p) (ExistsNext p') =
        eq p p'
      eq (ExistsUntil p q) (ExistsUntil p' q') =
        eq p p' /\ eq q q'
      eq (ExistsGlobally p) (ExistsGlobally p') =
        eq p p'
      eq _ _ = false

      variants = variant

      mapVariables mvf formula = case formula of
            Predicate a -> Predicate (mapVariables mvf a)
            Boolean a -> Boolean (mapVariables mvf a)
            IndexedDisjunction s -> IndexedDisjunction (mapVariables mvf s)
            Disjunction p q -> Disjunction (mapVariables mvf p) (mapVariables mvf q)
            Negation p -> Negation (mapVariables mvf p)
            ExistsNext p -> ExistsNext (mapVariables mvf p)
            ExistsUntil p q -> ExistsUntil (mapVariables mvf p) (mapVariables mvf q)
            ExistsGlobally p -> ExistsGlobally (mapVariables mvf p)


      foldVariables fvf acc formula = case formula of
            Predicate a -> foldVariables fvf acc a
            Boolean a -> foldVariables fvf acc a
            IndexedDisjunction s -> foldVariables fvf acc s
            Disjunction p q -> foldVariables fvf (foldVariables fvf acc p) q
            Negation p -> foldVariables fvf acc p
            ExistsNext p -> foldVariables fvf acc p
            ExistsUntil p q -> foldVariables fvf (foldVariables fvf acc p) q
            ExistsGlobally p -> foldVariables fvf acc p


graphRep :: Nominal a => (Atom -> a) -> Set (Atom, a)
graphRep f = NL.map (\a -> (a, f a)) NL.atoms
