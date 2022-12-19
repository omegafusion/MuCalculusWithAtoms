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
      var         { TokenVar $$ }
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

Formula     : mu Variable dot Formula { \a -> Mu ($2 a) ($4 a) }
            | nu Variable dot Formula { \a -> Negation (Mu ($2 a) (Negation (substitute ($2 a) (Negation (Variable ($2 a))) ($4 a)))) }
            | Formula1                { \a -> $1 a }

Formula1    : Formula2 or Formula1    { \a -> Disjunction ($1 a) ($3 a) }
            | Formula2 and Formula1   { \a -> Negation (Disjunction (Negation ($1 a)) (Negation ($3 a))) }
            | Formula2                { \a -> $1 a }

Formula2    : not Formula2            { \a -> Negation ($2 a) }
            | dia Formula2            { \a -> Diamond ($2 a) }
            | box Formula2            { \a -> Negation (Diamond (Negation ($2 a))) }
            | Formula3                { \a -> $1 a }

Formula3    : true                    { \a -> Boolean True }
            | false                   { \a -> Boolean False }
            | Predicate               { \a -> Predicate ($1 a) }
            | Variable                { \a -> Variable ($1 a) }
            | lpar Formula rpar       { \a -> $2 a }

Predicate   : pred Atoms              { \a -> (Pred $1 ($2 a)) }

Variable    : var Atoms               { \a -> (Var $1 ($2 a)) }

Atoms       :                         { \a -> [] }
            | underscore AtomList     { $2 }

AtomList    : atom                    { \a -> [$1] }
            | atom comma AtomList     { \a -> $1 : ($3 a) }

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
parser xs = calc (lexer xs) undefined

--main = getContents >>= print . calc . lexer
}