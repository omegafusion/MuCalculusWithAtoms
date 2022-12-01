module Syntax
    (Formula (..),
     Pred (..),
     Var (..),
     substitute) where


newtype Pred = Pred Int deriving (Show, Eq, Ord)

newtype Var = Var Int deriving (Show, Eq, Ord)

data Formula
      = Predicate Pred
      | Variable Var
      | Disjunction Formula Formula
      | Negation Formula
      | Diamond Formula
      | Mu Var Formula
      deriving (Show, Eq) -- Syntactic equality only


dual :: Formula -> Formula
dual (Disjunction p q) = Negation (Disjunction (Negation p) (Negation q))
dual (Diamond p) = Negation (Diamond (Negation p))
dual (Mu x p) = Negation (Mu x (Negation (substitute x (Negation (Variable x)) p)))


freevars :: Formula -> [Var]
freevars formula =
    let fvs xs f = case f of
                    Predicate p -> []
                    Variable y -> if (y `elem` xs) then [] else [y]
                    Disjunction p q -> fvs xs p ++ fvs xs q
                    Negation p -> fvs xs p
                    Mu x p -> fvs (x:xs) p
    in fvs [] formula


freshFrom :: [Var] -> Var
freshFrom xs = foldr (\(Var x) (Var y) -> if x>=y then Var (x+1) else Var y) (Var 0) xs


substitute :: Var -> Formula -> Formula -> Formula
substitute x t =
    -- might rename bound variables if they appear free in t
    -- find free variables in t
    let fv = freevars t
        sub (Variable y) =
              if x==y then t else Variable y
        sub (Predicate a) = Predicate a
        sub (Negation p) = Negation (sub p)
        sub (Disjunction p q) = Disjunction (sub p) (sub q)
        sub (Diamond p) = Diamond (sub p)
        sub (Mu y p) = if x==y then Mu y p -- x does not occur free in p
                       else if y `elem` fv then let z = freshFrom fv in Mu z (sub (substitute y (Variable z) t))
                       else Mu y (sub p)
            -- if the variable we're substituting is bound,
            -- it's not really the same variable  
    in sub
