module SyntaxUtils (
      Pred (..),
      graphRep,
      boundedGraphRep,
      conditionalBoundedGraphRep,
      constantAsGraph) where


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

boundedGraphRep :: Nominal a => Int -> ([Atom] -> a) -> Set ([Atom], a)
boundedGraphRep n f = NL.map (\as -> (as, f as)) $ NL.replicateAtoms n

constantAsGraph :: (Nominal a, Nominal b) => b -> Set ([a], b)
constantAsGraph p = NL.singleton ([], p)

elementToSingleton :: Nominal a => (Atom -> a) -> ([Atom] -> a)
elementToSingleton f [a] = f a

conditionalGraphRep :: Nominal a => (Atom -> NL.Formula) -> (Atom -> a) -> Set (Atom, a)
conditionalGraphRep cond f = NL.map (\a -> (a, f a)) $ NL.filter cond NL.atoms

conditionalBoundedGraphRep :: Nominal a => Int -> ([Atom] -> NL.Formula) -> ([Atom] -> a) -> Set ([Atom], a)
conditionalBoundedGraphRep n cond f = NL.map (\as -> (as, f as)) $ NL.filter cond $ NL.replicateAtoms n