{
module Parser (parser) where


import Data.Char (isDigit, isAlpha, isSpace)

import Lexer (Token (..),
              lexer)
import MuSyntax (Formula (..),
               Var (..),
               Pred (..),
               substitute)
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      pred        { TokenPred $$ }
      var         { TokenVar $$ }
      lpar        { TokenLPar }
      rpar        { TokenRPar }
      lcurl       { TokenLCurl }
      rcurl       { TokenRCurl }
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
      comma       { TokenComma }


%%

-- TODO: Duals

Formula     : mu var dot Formula                   { Mu $2 [($2, $4)] }
            | mu var dot lcurl FormulaList rcurl   { Mu $2 $5 }
            --| nu var dot Formula      { Negation (Mu $2 [($2, (Negation (substitute $2 (Negation (Variable $2)) $4)))]) }
            | Formula1                             { $1 }

FormulaList : var dot Formula         { [($1, $3)] }
            | var dot Formula comma FormulaList { ($1, $3) : $5 }

Formula1    : Formula2 or Formula1    { Disjunction $1 $3 }
            | Formula2 and Formula1   { Negation (Disjunction (Negation $1) (Negation $3)) }
            | Formula2                { $1 }

Formula2    : not Formula2            { Negation $2 }
            | dia Formula2            { Diamond $2 }
            | box Formula2            { Negation (Diamond (Negation $2)) }
            | Formula3                { $1 }

Formula3    : true                    { Disjunction (Predicate pred0) (Negation (Predicate pred0)) }
            | false                   { Negation (Disjunction (Predicate pred0) (Negation (Predicate pred0))) }
            | pred                    { Predicate $1 }
            | var                     { Variable $1 }
            | lpar Formula rpar       { $2 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"

pred0 = Pred 0


parser :: String -> Formula
parser = calc . lexer

main = getContents >>= print . calc . lexer
}