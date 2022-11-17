{
module Parser (parser) where


import Data.Char (isDigit, isAlpha, isSpace)

import Lexer (Token (..),
              lexer)
import Syntax (Formula (..),
               Var (..),
               Pred (..),
               substitute)

import NLambda (atom)
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      pred        { TokenPred $$ }
      label       { TokenLabel $$ }
      atom        { TokenAtom $$ }
      underscore  { TokenUnderscore }
      comma       { TokenComma }
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

Formula     : mu Variable dot Formula { Mu $2 $4 }
            | nu Variable dot Formula { Negation (Mu $2 (Negation (substitute $2 (Negation (Variable $2)) $4))) }
            | Formula1                { $1 }

Formula1    : Formula2 or Formula1    { Disjunction $1 $3 }
            | Formula2 and Formula1   { Negation (Disjunction (Negation $1) (Negation $3)) }
            | Formula2                { $1 }

Formula2    : not Formula2            { Negation $2 }
            | dia Formula2            { Diamond $2 }
            | box Formula2            { Negation (Diamond (Negation $2)) }
            | Formula3                { $1 }

Formula3    : true                    { Boolean True }
            | false                   { Boolean False }
            | pred                    { Predicate (Pred $1) }
            | Variable                { Variable $1 }
            | lpar Formula rpar       { $2 }

Variable    : label Atoms             { Var $1 $2 }

Atoms       :                         { [] }
            | underscore AtomList     { $2 }

AtomList    : atom                    { [$1] }
            | atom comma AtomList    { $1 : $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

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