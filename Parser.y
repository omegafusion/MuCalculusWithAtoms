{
module Parser (parser) where


import Data.Char (isDigit, isAlpha, isSpace)

import Data.Map (Map, (!), empty, insert)

import Lexer (Token (..),
              lexer)
              
import SyntaxUtils (Pred (..))
import MuSyntax (Var (..),
               negateVars,
               graphRep,
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

CTLFormula  : or under mvar Condition dot CTLFormula { \r -> CTL.IndexedDisjunction $ NL.map (\a -> (a, $6 (insert $3 a r))) ($4 r) }
            | and under mvar Condition dot CTLFormula { \r -> CTL.Negation $ CTL.IndexedDisjunction $ NL.map (\a -> (a, CTL.Negation $ $6 (insert $3 a r))) ($4 r) }
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

MuFormula   : mu Variable dot MuFormula { \r -> Mu.Mu ($2 r) ($2 r, $4 r) }
            | nu Variable dot MuFormula { \r -> Mu.Negation $ Mu.Mu ($2 r) ($2 r, Mu.Negation $ negateVars [$2 r] ($4 r)) }
            | mu lpar Variable rpar lcurl Variable2 dot MuFormula rcurl {
                  \r -> let (i, xs) = ($6 r)
                            r' = addFreshAtoms xs r
                            x = Var i (map NL.atom xs)
                        in Mu.Mu ($3 r) (x, $8 r') }
            | nu lpar Variable rpar lcurl Variable2 dot MuFormula rcurl { \r -> undefined }
            | or under mvar Condition dot MuFormula { \r -> Mu.IndexedDisjunction (freeLabels ($6 r), NL.map (\a -> (a, $6 (insert $3 a r))) ($4 r)) }
            | and under mvar Condition dot MuFormula { \r -> Mu.Negation $ Mu.IndexedDisjunction (freeLabels ($6 r), NL.map (\a -> (a, Mu.Negation $ $6 (insert $3 a r))) ($4 r)) }
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
Variable2   : var Atoms2              { \r -> ($1, $2 r) }

Atoms       :                         { const [] }
            | under AtomList          { $2 }
Atoms2      :                         { const [] }
            | under AtomList2         { $2 }

AtomList    : Atom                    { \r -> [$1 r] }
            | Atom comma AtomList     { \r -> ($1 r) : ($3 r) }
AtomList2   : Atom2                   { \r -> [$1 r] }
            | Atom2 comma AtomList2   { \r -> ($1 r) : ($3 r) }

Atom        : atom                    { const $1 }
            | mvar                    { (! $1) }
Atom2       : mvar                    { const $1 }

{
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