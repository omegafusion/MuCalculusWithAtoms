{
module Lexer (
  Token (..),
  lexer
) where

import Syntax (
  Pred (..),
  Var (..))

import NLambda (Atom, atom)
}


%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+		    ;
  "p"$alpha+    { \s -> TokenPred (atom (tail s)) }
  "v"$alpha+    { \s -> TokenLabel (tail s) }
  "_"           { \s -> TokenUnderscore }
  ","           { \s -> TokenComma }
  "("           { \s -> TokenOB }
  ")"           { \s -> TokenCB }
  "~"           { \s -> TokenNeg }
  "|"           { \s -> TokenDisj }
  "&"           { \s -> TokenConj }
  true          { \s -> TokenTrue }
  false         { \s -> TokenFalse }
  "<>"          { \s -> TokenDia }
  "[]"          { \s -> TokenBox }
  mu            { \s -> TokenMu }
  nu            { \s -> TokenNu }
  "."           { \s -> TokenDot }
  $alpha+       { \s -> TokenAtom (atom s) }

{
-- Each action has type :: String -> Token

-- The token type:
data Token 
      = TokenPred Atom
      | TokenLabel String
      | TokenAtom Atom
      | TokenUnderscore
      | TokenComma
      | TokenOB      
      | TokenCB      
      | TokenNeg     
      | TokenDisj    
      | TokenConj    
      | TokenTrue    
      | TokenFalse
      | TokenDia
      | TokenBox
      | TokenMu
      | TokenNu
      | TokenDot
      deriving (Eq,Show)


lexer :: String -> [Token]
lexer = alexScanTokens

--main = do
--  s <- getContents
--  print (alexScanTokens s)
}