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
  "p"$digit+    { \s -> TokenPred (read (tail s)) }
  "v"$digit+    { \s -> TokenVar (read (tail s)) }
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