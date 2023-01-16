module SyntaxUtils (Pred (..)) where


import NLambda ( Atom, Nominal, (/\), eq, variants, variant, mapVariables, foldVariables )


data Pred = Pred Int [Atom] deriving (Show, Eq, Ord)

instance Nominal Pred where
      eq (Pred n as) (Pred n' as') = eq n n' /\ eq as as'
      variants = variant
      mapVariables mvf (Pred n as) = Pred n (mapVariables mvf as)
      foldVariables fvf acc (Pred n as) = foldVariables fvf acc as