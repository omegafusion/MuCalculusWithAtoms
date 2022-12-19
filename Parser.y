{
module Parser (parser) where


import Data.Char (isDigit, isAlpha, isSpace)

import Data.Map ((!), empty, insert)

import Lexer (Token (..),
              lexer)
import Syntax (Formula (..),
               Var (..),
               Pred (..),
               substitute,
               graphRep)

import NLambda (atom)
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      pred        { TokenPred $$ }
      var         { TokenVar $$ }
      atom        { TokenAtom $$ }
      mvar        { TokenMVar $$ }
      under       { TokenUnderscore }
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

Formula     : mu Variable dot Formula { \r -> Mu ($2 r) ($4 r) }
            | nu Variable dot Formula { \r -> Negation $ Mu ($2 r) (Negation $ substitute ($2 r) (Negation $ Variable ($2 r)) ($4 r)) }
            | or under mvar dot Formula { \r -> IndexedDisjunction $ graphRep $ \a -> $5 (insert $3 a r) }
            | Formula1                { $1 }

Formula1    : Formula2 or Formula1    { \r -> Disjunction ($1 r) ($3 r) }
            | Formula2 and Formula1   { \r -> Negation $ Disjunction (Negation ($1 r)) (Negation ($3 r)) }
            | Formula2                { $1 }

Formula2    : not Formula2            { Negation . $2 }
            | dia Formula2            { Diamond . $2 }
            | box Formula2            { Negation . Diamond . Negation . $2 }
            | Formula3                { $1 }

Formula3    : true                    { const $ Boolean True }
            | false                   { const $ Boolean False }
            | Predicate               { Predicate . $1 }
            | Variable                { Variable . $1 }
            | lpar Formula rpar       { $2 }

Predicate   : pred Atoms              { Pred $1 . $2 }

Variable    : var Atoms               { Var $1 . $2 }

Atoms       :                         { const [] }
            | under AtomList          { $2 }

AtomList    : Atom                    { \r -> [$1 r] }
            | Atom comma AtomList     { \r -> ($1 r) : ($3 r) }

Atom        : atom                    { const $1 }
            | mvar                    { (! $1) }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parser :: String -> Formula
parser xs = calc (lexer xs) empty

--main = getContents >>= print . calc . lexer
}