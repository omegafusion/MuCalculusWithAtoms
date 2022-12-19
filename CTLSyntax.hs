module CTLSyntax (
    StateFormula (..),
    PathFormula (..),
    Pred (..)
) where


newtype Pred = Pred Int deriving (Show, Eq, Ord)


data StateFormula
    = True
    | Predicate Pred
    | Conjunction StateFormula StateFormula
    | Negation StateFormula
    | Exists PathFormula
    deriving (Eq, Ord, Show)

data PathFormula
    = Next StateFormula
    | Until StateFormula StateFormula
    | Globally StateFormula
    deriving (Eq, Ord, Show)