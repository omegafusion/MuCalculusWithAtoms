module MuSyntax
    (Formula (..),
     Pred (..),
     Var (..),
     substitute
) where

import Data.List ( elemIndex )

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


filterByVar :: Var -> FormulaVector -> FormulaVector
filterByVar x = filter ((==x) . fst)

findIndex :: Var -> FormulaVector -> Maybe Int
findIndex x vector = elemIndex x (map fst vector)

getByVar :: Var -> FormulaVector -> Formula
getByVar x = snd . head . filterByVar x

isLegal :: Var -> FormulaVector -> Bool
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
    Mu _ [(x, p)] == Mu _ [(x', p')] =
        let fv = freeVars p ++ freeVars p'
            y = freshFrom fv
        in nameswap x y p == nameswap x' y p'


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
                    Mu _ [(x, p)] -> fvs (x:xs) p
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
            Mu _ [(z, p)] -> let z' = ns x y z in Mu z' [(z', nameswap x y p)]


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
        sub (Mu _ [(y, p)]) = if x==y then Mu y [(y, p)] -- x does not occur free in p
                       else if y `elem` fv then let z = freshFrom fv in Mu z [(z, sub (substitute y (Variable z) t))]
                       else Mu y [(y, sub p)]
            -- if the variable we're substituting is bound,
            -- it's not really the same variable  
    in sub
