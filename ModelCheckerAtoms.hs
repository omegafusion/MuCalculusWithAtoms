module ModelCheckerAtoms (
    check
)
where

import ModelCheckerUtils (State, KripkeModel)

import qualified CTLModelCheckerAtoms as CTL
import qualified CTLSyntax as CTL
import qualified MuModelCheckerAtoms as Mu
import qualified MuSyntax as Mu

import NLambda (Set)

type EitherFormula = Either Mu.Formula CTL.Formula

check :: KripkeModel -> EitherFormula -> Set State
check model p = case p of
    Left phi -> Mu.check model phi
    Right phi -> CTL.check model phi
