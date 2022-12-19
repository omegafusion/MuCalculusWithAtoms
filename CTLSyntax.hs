module CTLSyntax (
    Formula (..),
    Pred (..)
) where


newtype Pred = Pred Int deriving (Show, Eq, Ord)


data Formula
    = True
    | Predicate Pred
    | Conjunction Formula Formula
    | Negation Formula
    | ExistsNext Formula
    | ExistsUntil Formula Formula
    | ExistsGlobally Formula
    deriving (Eq, Ord, Show)
