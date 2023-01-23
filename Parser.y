{
module Parser (parser) where


import Data.Char (isDigit, isAlpha, isSpace)

import Data.Map ((!), empty, insert)

import Lexer (Token (..),
              lexer)
              
import SyntaxUtils (Pred (..))
import MuSyntax (Formula (..),
               Var (..),
               negateVars,
               graphRep)

import NLambda (atom, atoms, difference, fromList)
import qualified NLambda as NL
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
      lpar        { TokenLPar }
      rpar        { TokenRPar }
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
      neq         { TokenNeq }
      lt          { TokenLT }
      gt          { TokenGT }


%%

-- TODO: Duals

Formula     : MuFormula { $1 }

MuFormula   : mu Variable dot MuFormula { \r -> Mu ($2 r) ($4 r) }
            | nu Variable dot MuFormula { \r -> Negation $ Mu ($2 r) (Negation $ negateVars [$2 r] ($4 r)) }
            | or under mvar Condition dot MuFormula { \r -> IndexedDisjunction $ NL.map (\a -> (a, $6 (insert $3 a r))) ($4 r) }
            | and under mvar Condition dot MuFormula { \r -> Negation $ IndexedDisjunction $ NL.map (\a -> (a, Negation $ $6 (insert $3 a r))) ($4 r) }
            | MuFormula1                { $1 }

Condition   :                        { const atoms }    
            | neq Atom               { \r -> NL.filter (`NL.neq` ($2 r)) atoms }
            | lt Atom                { \r -> NL.filter (`NL.lt` ($2 r)) atoms }
            | gt Atom                { \r -> NL.filter (`NL.gt` ($2 r)) atoms }

MuFormula1  : MuFormula2 or MuFormula1  { \r -> Disjunction ($1 r) ($3 r) }
            | MuFormula2 and MuFormula1 { \r -> Negation $ Disjunction (Negation ($1 r)) (Negation ($3 r)) }
            | MuFormula2                { $1 }

MuFormula2  : not MuFormula2          { Negation . $2 }
            | dia MuFormula2          { Diamond . $2 }
            | box MuFormula2          { Negation . Diamond . Negation . $2 }
            | MuFormula3              { $1 }

MuFormula3  : true                    { const $ Boolean True }
            | false                   { const $ Boolean False }
            | MuPredicate             { Predicate . $1 }
            | Variable                { Variable . $1 }
            | lpar MuFormula rpar     { $2 }

MuPredicate : pred Atoms              { Pred $1 . $2 }

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