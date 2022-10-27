{
module Parser (parser) where


import Data.Char (isDigit, isAlpha, isSpace)

import Lexer (Token (..),
              lexer)
import Syntax (Formula (..),
               AtomicFormula (..),
               Var (..),
               Pred (..),
               negation,
               substitute)
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      pred        { TokenPred $$ }
      var         { TokenVar $$ }
      lpar        { TokenOB }
      rpar        { TokenCB }
      not         { TokenNeg }
      or          { TokenDisj }
      and         { TokenConj }
      true        { TokenTrue }
      false       { TokenFalse }
      dia         { TokenDia }
      box         { TokenBox }
      mu          { TokenMu }
      nu          { TokenNu }
      dot         { TokenDot }


%%

-- TODO: Duals

Formula     : mu var dot Formula      { Mu $2 $4 }
            | nu var dot Formula      { Nu $2 $4 }
            | Formula1                { $1 }

Formula1    : Formula2 or Formula1    { Disjunction $1 $3 }
            | Formula2 and Formula1   { Conjunction $1 $3 }
            | Formula2                { $1 }

Formula2    : not Formula2            { negation $2 }
            | dia Formula2            { Diamond $2 }
            | box Formula2            { Box $2 }
            | Formula3                { $1 }

Formula3    : AtomicForm              { Positive $1 }
            | TruthValue              { $1 }
            | lpar Formula rpar       { $2 }

AtomicForm  : pred                    { Predicate $1 }
            | var                     { Variable $1 }
            -- TODO: True and false

TruthValue  : true                    { Disjunction (Positive (Predicate pred0)) (Negative (Predicate pred0)) }
            | false                   { Conjunction (Positive (Predicate pred0)) (Negative (Predicate pred0)) }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"

pred0 = Pred 0

--data Exp  
--      = Let String Exp Exp
--      | Exp1 Exp1
--      deriving Show
--
--data Exp1 
--      = Plus Exp1 Term 
--      | Minus Exp1 Term 
--      | Term Term
--      deriving Show
--
--data Term 
--      = Times Term Factor 
--      | Div Term Factor 
--      | Factor Factor
--      deriving Show
--
--data Factor 
--      = Int Int 
--      | Var String 
--      | Brack Exp
--      deriving Show

--data Token
--      = TokenLet
--      | TokenIn
--      | TokenInt Int
--      | TokenVar String
--      | TokenEq
--      | TokenPlus
--      | TokenMinus
--      | TokenTimes
--      | TokenDiv
--      | TokenOB
--      | TokenCB
-- deriving Show

--lexer [] = []
--lexer (c:cs) 
--      | isSpace c = lexer cs
--      | isAlpha c = lexVar (c:cs)
--      | isDigit c = lexNum (c:cs)
--lexer ('=':cs) = TokenEq : lexer cs
--lexer ('+':cs) = TokenPlus : lexer cs
--lexer ('-':cs) = TokenMinus : lexer cs
--lexer ('*':cs) = TokenTimes : lexer cs
--lexer ('/':cs) = TokenDiv : lexer cs
--lexer ('(':cs) = TokenOB : lexer cs
--lexer (')':cs) = TokenCB : lexer cs

--lexNum cs = TokenInt (read num) : lexer rest
--      where (num,rest) = span isDigit cs
--
--lexVar cs =
--   case span isAlpha cs of
--      ("let",rest) -> TokenLet : lexer rest
--      ("in",rest)  -> TokenIn : lexer rest
--      (var,rest)   -> TokenVar var : lexer rest

parser :: String -> Formula
parser = calc . lexer

main = getContents >>= print . calc . lexer
}