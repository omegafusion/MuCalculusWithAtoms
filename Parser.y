{
module Parser (parser) where


import Data.Char (isDigit, isAlpha, isSpace)

import Data.Map (Map, (!), empty, insert)

import Lexer (
      Token (..),
      lexer)
              
import SyntaxUtils (
      Pred (..),
      boundedGraphRep,
      conditionalBoundedGraphRep)
import MuSyntax (
      Var (..),
      negateVars,
      freeLabels)
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
      lcurl       { TokenLCurl }
      rcurl       { TokenRCurl }
      not         { TokenNeg }
      or          { TokenDisj }
      and         { TokenConj }
      true        { TokenTrue }
      false       { TokenFalse }
      dia         { TokenDia }
      box         { TokenBox }
      forall      { TokenForAll }
      exists      { TokenExists }
      next        { TokenNext }
      finally     { TokenFinally }
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

CTLFormula  : or under mvar Condition dot CTLFormula { \r -> CTL.IndexedDisjunction $ conditionalBoundedGraphRep 1 ($4 r) (\[a] -> $6 (insert $3 a r)) }
            | and under mvar Condition dot CTLFormula { \r -> CTL.Negation $ CTL.IndexedDisjunction $ conditionalBoundedGraphRep 1 ($4 r) (\[a] -> CTL.Negation $ $6 (insert $3 a r)) }
            | CTLFormula1               { $1 }

CTLFormula1 : CTLFormula2 or CTLFormula1  { \r -> CTL.Disjunction ($1 r) ($3 r) }
            | CTLFormula2 and CTLFormula1 { \r -> CTL.Negation $ CTL.Disjunction (CTL.Negation ($1 r)) (CTL.Negation ($3 r)) }
            | CTLFormula2                 { $1 }

CTLFormula2 : not CTLFormula2             { CTL.Negation . $2 }
            | exists next CTLFormula2     { \r -> CTL.ExistsNext ($3 r) }
            | exists finally CTLFormula2  { \r -> CTL.ExistsUntil (CTL.Boolean True) ($3 r) }
            | exists globally CTLFormula2 { \r -> CTL.ExistsGlobally ($3 r) }
            | exists lpar CTLFormula2 until CTLFormula2 rpar { \r -> CTL.ExistsUntil ($3 r) ($5 r) }
            | forall next CTLFormula2     { \r -> CTL.Negation $ CTL.ExistsNext $ CTL.Negation ($3 r) }
            | forall finally CTLFormula2  { \r -> CTL.Negation $ CTL.ExistsUntil (CTL.Boolean True) (CTL.Negation ($3 r)) }
            | forall globally CTLFormula2 { \r -> CTL.Negation $ CTL.ExistsGlobally $ CTL.Negation ($3 r) }
            | forall lpar CTLFormula2 until CTLFormula2 rpar
                  { \r -> CTL.Negation $ CTL.Disjunction (CTL.ExistsUntil (CTL.Negation ($5 r)) (CTL.Negation $ CTL.Disjunction ($3 r) ($5 r))) (CTL.ExistsGlobally $ CTL.Negation ($5 r)) }
            | CTLFormula3                 { $1 }

CTLFormula3 : true                    { const $ CTL.Boolean True }
            | false                   { const $ CTL.Boolean False }
            | Predicate               { CTL.Predicate . $1 }
            | lpar CTLFormula rpar    { $2 }

MuFormula   : mu Variable dot MuFormula { \r ->
                  let formulaSet = NL.singleton ([], ($4 r))
                  in Mu.Mu ($2 r) (freeLabels ($4 r), formulaSet) }
            | nu Variable dot MuFormula { \r ->
                  let formulaSet = NL.singleton ([], Mu.Negation ($4 r))
                  in Mu.Negation $ Mu.Mu ($2 r) (freeLabels ($4 r), formulaSet) }
            | mu Variable lcurl MVarList dot MuFormula rcurl { \r ->
                  let formulaSet = boundedGraphRep (length $4) (\as -> $6 (insertFromLists $4 as r))
                  in Mu.Mu ($2 r) (freeLabels ($6 r), formulaSet)
              }
            | nu Variable lcurl MVarList dot MuFormula rcurl { \r ->
                  let formulaSet = boundedGraphRep (length $4) (\as -> Mu.Negation $ negateVars [Mu.label ($2 r)] $ $6 (insertFromLists $4 as r))
                  in Mu.Negation $ Mu.Mu ($2 r) (freeLabels ($6 r), formulaSet)
              }
            | or under mvar Condition dot MuFormula { \r ->
                  Mu.IndexedDisjunction (freeLabels ($6 r), conditionalBoundedGraphRep 1 ($4 r) (\[a] -> $6 (insert $3 a r)))
              }
            | and under mvar Condition dot MuFormula { \r -> 
                  Mu.Negation $ Mu.IndexedDisjunction (freeLabels ($6 r), conditionalBoundedGraphRep 1 ($4 r) (\[a] ->  Mu.Negation $ $6 (insert $3 a r)))
              }
            | MuFormula1                { $1 }

Condition   :                        { \r [a] -> NL.true }    
            | neq Atom               { \r [a] -> a `NL.neq` ($2 r) }
            | lt Atom                { \r [a] -> a `NL.lt` ($2 r) }
            | gt Atom                { \r [a] -> a `NL.gt` ($2 r) }

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

MVarList    : mvar                    { [$1] }
            | mvar comma MVarList     { $1 : $3 }

Atom        : atom                    { const $1 }
            | mvar                    { (! $1) }

{
insertFromLists :: Ord k => [k] -> [a] -> Map k a -> Map k a
insertFromLists ks as r = foldr (\(k, a) -> insert k a) r (zip ks as)

parseError :: [Token] -> a
parseError _ = error "Parse error"

addFreshAtoms :: [String] -> Map String NL.Atom -> Map String NL.Atom
addFreshAtoms xs r =
      let pairs = map (\a -> (a, NL.atom a)) xs
      in foldr (uncurry insert) r pairs

parser :: String -> Either Mu.Formula CTL.Formula
parser = calc . lexer

--main = getContents >>= print . calc . lexer
}