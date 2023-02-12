module ModelCheckerUtils (
    State (..),
    TransRel,
    SatRel,
    KripkeModel
) where


import NLambda ( (/\), variant, Atom, Nominal(..), Set )
import SyntaxUtils ( Pred )

-- TODO: Arbritrary sets with atoms?

data State = State Int [Atom] deriving (Show, Eq, Ord)

instance Nominal State where
    eq (State i as) (State i' as') = i `eq` i' /\ as `eq` as'
    variants = variant
    mapVariables mvf (State i as) = State i (mapVariables mvf as)
    foldVariables fvf acc (State i as) = foldVariables fvf acc as


type TransRel = Set (State, State)

type SatRel = Set (State, Pred)

type KripkeModel = (Set State, TransRel, SatRel)
-- A Kripke model is a triple consisting of a state set, a transition relation and a satisfaction relation