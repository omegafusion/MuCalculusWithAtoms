{
module Lexer (
  Token (..),
  lexer
) where

import Syntax (
  Pred (..),
  Var (..))
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+		    ;
  "p"$digit+    { \s -> TokenPred (Pred $ read (tail s)) }
  "v"$digit+    { \s -> TokenVar (Var $ read (tail s)) }
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

{
-- Each action has type :: String -> Token

-- The token type:
data Token 
      = TokenPred Pred
      | TokenVar Var 
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