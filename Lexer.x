{
module Lexer (
  Token (..),
  lexer
) where

import Syntax (
  Pred (..),
  Var (..))

import NLambda (Atom, atom, constant)
}


%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+		    ;
  "p"$digit+    { \s -> TokenPred (atomFromString (tail s)) }
  "v"$digit+    { \s -> TokenLabel (read (tail s)) }
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
  $digit+       { \s -> TokenAtom (atomFromString s) }

{
-- Each action has type :: String -> Token

-- The token type:
data Token 
      = TokenPred Atom
      | TokenLabel Int
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


atomFromString :: String -> Atom
atomFromString = constant . read


lexer :: String -> [Token]
lexer = alexScanTokens

--main = do
--  s <- getContents
--  print (alexScanTokens s)
}