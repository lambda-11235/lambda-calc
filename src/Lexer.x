{
module Lexer (Token (..), LexOut (..), scan) where
}

%wrapper "posn"

@character = [a-zA-Z]

tokens :-

  $white+                               ;
  "("                                   { \p s -> lexOut p LLParen }
  ")"                                   { \p s -> lexOut p LRParen }
  -- FIXME: \x. x is not working
  "\"                                  { \p s -> lexOut p LLambda }
  "λ"                                   { \p s -> lexOut p LLambda }
  "."                                   { \p s -> lexOut p LDot }
  @character+                           { \p s -> lexOut p (LVar s) }

{
data Token = LLParen
           | LRParen
           | LLambda
           | LDot
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
