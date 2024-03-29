{
module Lexer (
  Token (..),
  lexer
) where

import MuSyntax ( Var (..) )
import SyntaxUtils ( Pred (..) )

import NLambda (Atom, atom, constant)
}


%wrapper "basic"

$digit = 0-9			  -- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+		    ;
  "p"$digit+    { \s -> TokenPred (read (tail s)) }
  "v"$digit+    { \s -> TokenVar (read (tail s)) }
  "_"           { \s -> TokenUnderscore }
  ","           { \s -> TokenComma }
  "("           { \s -> TokenLPar }
  ")"           { \s -> TokenRPar }
  "["           { \s -> TokenLBrack }
  "]"           { \s -> TokenRBrack }
  "{"           { \s -> TokenLCurl }
  "}"           { \s -> TokenRCurl }
  "~"           { \s -> TokenNeg }
  "|"           { \s -> TokenDisj }
  "&"           { \s -> TokenConj }
  true          { \s -> TokenTrue }
  false         { \s -> TokenFalse }
  "<>"          { \s -> TokenDia }
  "[]"          { \s -> TokenBox }
  "A"           { \s -> TokenForAll }
  "E"           { \s -> TokenExists }
  "X"           { \s -> TokenNext }
  "F"           { \s -> TokenFinally }
  "G"           { \s -> TokenGlobally }
  "U"           { \s -> TokenUntil }
  mu            { \s -> TokenMu }
  nu            { \s -> TokenNu }
  "."           { \s -> TokenDot }
  "/="          { \s -> TokenNeq }
  "<"           { \s -> TokenLT }
  ">"           { \s -> TokenGT }
  "C"           { \s -> TokenCTLMark }
  "M"           { \s -> TokenMuMark }
  $digit+       { \s -> TokenAtom (atomFromString s) }
  $alpha+       { \s -> TokenMVar s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token 
      = TokenPred Int
      | TokenVar Int
      | TokenAtom Atom
      | TokenMVar String
      | TokenUnderscore
      | TokenComma
      | TokenLPar
      | TokenRPar
      | TokenLBrack
      | TokenRBrack
      | TokenLCurl
      | TokenRCurl
      | TokenNeg     
      | TokenDisj    
      | TokenConj    
      | TokenTrue    
      | TokenFalse
      | TokenDia
      | TokenBox
      | TokenForAll
      | TokenExists
      | TokenNext
      | TokenFinally
      | TokenGlobally
      | TokenUntil
      | TokenMu
      | TokenNu
      | TokenDot
      | TokenNeq
      | TokenLT
      | TokenGT
      | TokenCTLMark
      | TokenMuMark
      deriving (Eq,Show)


atomFromString :: String -> Atom
atomFromString = constant . toRational . read


lexer :: String -> [Token]
lexer = alexScanTokens
}