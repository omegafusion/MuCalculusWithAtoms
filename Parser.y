{
module Parser (parser) where


import Data.Char (isDigit, isAlpha, isSpace)

import Data.Map ((!), empty, insert)

import Lexer (Token (..),
              lexer)
              
import SyntaxUtils (Pred (..))
import MuSyntax (Var (..),
               negateVars,
               graphRep)
import qualified MuSyntax as Mu

import qualified CTLSyntax as CTL

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
      lbrack      { TokenLBrack }
      rbrack      { TokenRBrack }
      not         { TokenNeg }
      or          { TokenDisj }
      and         { TokenConj }
      true        { TokenTrue }
      false       { TokenFalse }
      dia         { TokenDia }
      box         { TokenBox }
      exists      { TokenExists }
      next        { TokenNext }
      globally    { TokenGlobally }
      until       { TokenUntil }
      mu          { TokenMu }
      nu          { TokenNu }
      dot         { TokenDot }
      neq         { TokenNeq }
      lt          { TokenLT }
      gt          { TokenGT }
      c           { TokenCTLMark }
      m           { TokenMuMark }


%%

-- TODO: Duals

Formula     : m lbrack MuFormula rbrack { Left ($3 empty) }
            | c lbrack CTLFormula rbrack { Right ($3 empty) }

CTLFormula  : or under mvar Condition dot CTLFormula { \r -> CTL.IndexedDisjunction $ NL.map (\a -> (a, $6 (insert $3 a r))) ($4 r) }
            | and under mvar Condition dot CTLFormula { \r -> CTL.Negation $ CTL.IndexedDisjunction $ NL.map (\a -> (a, CTL.Negation $ $6 (insert $3 a r))) ($4 r) }
            | CTLFormula1               { $1 }

CTLFormula1 : CTLFormula2 or CTLFormula1  { \r -> CTL.Disjunction ($1 r) ($3 r) }
            | CTLFormula2 and CTLFormula1 { \r -> CTL.Negation $ CTL.Disjunction (CTL.Negation ($1 r)) (CTL.Negation ($3 r)) }
            | CTLFormula2                 { $1 }

CTLFormula2 : not CTLFormula2             { CTL.Negation . $2 }
            | exists CTLPathFormula       { $2 }
            | CTLFormula3                 { $1 }

CTLFormula3 : true                    { const $ CTL.Boolean True }
            | false                   { const $ CTL.Boolean False }
            | Predicate               { CTL.Predicate . $1 }
            | lpar CTLFormula rpar    { $2 }

CTLPathFormula : next CTLFormula2     { \r -> CTL.ExistsNext ($2 r) }
               | globally CTLFormula2 { \r -> CTL.ExistsGlobally ($2 r) }
               | lpar CTLFormula2 until CTLFormula2 rpar { \r -> CTL.ExistsUntil ($2 r) ($4 r) }

MuFormula   : mu Variable dot MuFormula { \r -> Mu.Mu ($2 r) ($4 r) }
            | nu Variable dot MuFormula { \r -> Mu.Negation $ Mu.Mu ($2 r) (Mu.Negation $ negateVars [$2 r] ($4 r)) }
            | or under mvar Condition dot MuFormula { \r -> Mu.IndexedDisjunction $ NL.map (\a -> (a, $6 (insert $3 a r))) ($4 r) }
            | and under mvar Condition dot MuFormula { \r -> Mu.Negation $ Mu.IndexedDisjunction $ NL.map (\a -> (a, Mu.Negation $ $6 (insert $3 a r))) ($4 r) }
            | MuFormula1                { $1 }

Condition   :                        { const atoms }    
            | neq Atom               { \r -> NL.filter (`NL.neq` ($2 r)) atoms }
            | lt Atom                { \r -> NL.filter (`NL.lt` ($2 r)) atoms }
            | gt Atom                { \r -> NL.filter (`NL.gt` ($2 r)) atoms }

MuFormula1  : MuFormula2 or MuFormula1  { \r -> Mu.Disjunction ($1 r) ($3 r) }
            | MuFormula2 and MuFormula1 { \r -> Mu.Negation $ Mu.Disjunction (Mu.Negation ($1 r)) (Mu.Negation ($3 r)) }
            | MuFormula2                { $1 }

MuFormula2  : not MuFormula2          { Mu.Negation . $2 }
            | dia MuFormula2          { Mu.Diamond . $2 }
            | box MuFormula2          { Mu.Negation . Mu.Diamond . Mu.Negation . $2 }
            | MuFormula3              { $1 }

MuFormula3  : true                    { const $ Mu.Boolean True }
            | false                   { const $ Mu.Boolean False }
            | Predicate               { Mu.Predicate . $1 }
            | Variable                { Mu.Variable . $1 }
            | lpar MuFormula rpar     { $2 }

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

parser :: String -> Either Mu.Formula CTL.Formula
parser = calc . lexer

--main = getContents >>= print . calc . lexer
}