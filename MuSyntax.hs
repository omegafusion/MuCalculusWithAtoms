module MuSyntax
    (Formula (..),
     Pred (..),
     Var (..),
     substitute,
     getByVar
) where

import Data.List ( elemIndex )
import Data.Maybe ( fromJust )

newtype Pred = Pred Int deriving (Show, Eq, Ord)

newtype Var = Var Int deriving (Show, Eq, Ord)

type FormulaVector = [(Var, Formula)]

data Formula
      = Predicate Pred
      | Variable Var
      | Disjunction Formula Formula
      | Negation Formula
      | Diamond Formula
      | Mu Var FormulaVector
      deriving (Show) -- Syntactic equality only


filterByVar :: Var -> [(Var, a)] -> [(Var, a)]
filterByVar x = filter ((==x) . fst)

findIndex :: Var -> [(Var, a)] -> Maybe Int
findIndex x vector = elemIndex x (map fst vector)

getByVar :: Var -> [(Var, a)] -> a
getByVar x = snd . head . filterByVar x

isLegal :: Var -> [(Var, a)] -> Bool
isLegal x vector = length (filterByVar x vector) == 1


instance Eq Formula where
    Predicate a == Predicate b =
        a == b
    Variable x == Variable y =
        x == y
    Disjunction p q == Disjunction p' q' =
        p == p' && q == q'
    Negation p == Negation p' =
        p == p'
    Diamond p == Diamond p' =
        p == p'
    Mu z ps == Mu z' ps' =
        let checkRow (x, p) (x', p') =
                let fv = freeVars p ++ freeVars p'
                    y = freshFrom fv
                in nameswap x y p == nameswap x' y p'
        in length ps == length ps' && and (zipWith checkRow ps ps') && findIndex z ps == findIndex z' ps'


{-dual :: Formula -> Formula
dual (Disjunction p q) = Negation (Disjunction (Negation p) (Negation q))
dual (Diamond p) = Negation (Diamond (Negation p))
dual (Mu x p) = Negation (Mu x (Negation (substitute x (Negation (Variable x)) p)))-}


freeVars :: Formula -> [Var]
freeVars formula =
    let fvs xs f = case f of
                    Predicate p -> []
                    Variable y -> if y `elem` xs then [] else [y]
                    Disjunction p q -> fvs xs p ++ fvs xs q
                    Negation p -> fvs xs p
                    Diamond p -> fvs xs p
                    Mu _ ps -> concatMap (\(x, p) -> fvs (x:xs) p) ps
    in fvs [] formula


freshFrom :: [Var] -> Var
freshFrom = foldl (\(Var x) (Var y) -> if x>=y then Var (x+1) else Var y) (Var 0)


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
            Mu z ps -> let z' = ns x y z
                           nameswapRow (w, p) = (ns x y w, nameswap x y p)
                       in Mu z' (map nameswapRow ps)


substitute :: Var -> Formula -> Formula -> Formula
substitute x t =
    -- might rename bound variables if they appear free in t
    -- find free variables in t
    let fv = freeVars t
        sub (Variable y) =
              if x==y then t else Variable y
        sub (Predicate a) = Predicate a
        sub (Negation p) = Negation (sub p)
        sub (Disjunction p q) = Disjunction (sub p) (sub q)
        sub (Diamond p) = Diamond (sub p)
        sub (Mu x ps) =
            let subRow (y, p)
                    | x == y       = (y, p)
                    | y `elem` fv  = let z = freshFrom fv in (z, sub (substitute y (Variable z) t))
                    | otherwise    = (y, sub p)
                ps' = map subRow ps
                x' = fst (ps' !! fromJust (findIndex x ps))
            in Mu x' ps'
    in sub
