module ModelCheckerUtils (
    State (..),
    TransRel,
    SatRel,
    KripkeModel
) where


import NLambda ( (/\), variant, Atom, Nominal(..), Set )
import qualified NLambda as NL
import SyntaxUtils ( Pred )

data State = State Int [Atom] deriving (Show, Eq, Ord) -- TODO: Make into a set with atoms

instance Nominal State where
    
    eq (State i as) (State i' as') = i `eq` i' /\ as `eq` as'

    variants = variant

    mapVariables mvf (State i as) = State i (mapVariables mvf as)
    
    foldVariables fvf acc (State i as) = foldVariables fvf acc as


type TransRel = Set (State, State) -- TODO: Convert to NLambda set

type SatRel = Set (State, Pred) -- TODO: Convert to NLambda set

type KripkeModel = (Set State, TransRel, SatRel) -- TODO: Convert to NLambda set
-- A Kripke model is a triple consisting of a state set, a transition relation and a satisfaction relation