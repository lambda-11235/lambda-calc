{
module Lexer (Token (..), LexOut (..), scan) where
}

%wrapper "posn"

@character = [a-zA-Z]

tokens :-

  $white+                               ;
  "("                                   { \p s -> lexOut p LLParen }
  ")"                                   { \p s -> lexOut p LRParen }
  "\"                                   { \p s -> lexOut p LLambda }
  "λ"                                   { \p s -> lexOut p LLambda }
  "let"                                 { \p s -> lexOut p LLet }
  "."                                   { \p s -> lexOut p LDot }
  "="                                   { \p s -> lexOut p LEqual }
  @character+                           { \p s -> lexOut p (LVar s) }

{
data Token = LLParen
           | LRParen
           | LLambda
           | LLet
           | LDot
           | LEqual
           | LVar String
           deriving (Eq, Show)

data LexOut = LexOut { offset :: Int
                     , line :: Int
                     , column :: Int
                     , getToken :: Token }
              deriving (Eq, Show)

lexOut (AlexPn offset line col) tok = LexOut offset line col tok

scan = alexScanTokens
}
