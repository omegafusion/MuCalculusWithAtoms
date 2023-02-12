module SyntaxUtils (
      Pred (..),
      graphRep,
      conditionalGraphRep) where


import NLambda (Atom, Nominal, Set, (/\), eq, variants, variant, mapVariables, foldVariables)
import qualified NLambda as NL


data Pred = Pred Int [Atom] deriving (Show, Eq, Ord)

instance Nominal Pred where

      eq (Pred n as) (Pred n' as') = eq n n' /\ eq as as'

      variants = variant

      mapVariables mvf (Pred n as) = Pred n (mapVariables mvf as)

      foldVariables fvf acc (Pred n as) = foldVariables fvf acc as


graphRep :: Nominal a => (Atom -> a) -> Set (Atom, a)
graphRep f = NL.map (\a -> (a, f a)) NL.atoms

conditionalGraphRep :: Nominal a => (Atom -> NL.Formula) -> (Atom -> a) -> Set (Atom, a)
conditionalGraphRep cond f = NL.map (\a -> (a, f a)) $ NL.filter cond NL.atoms