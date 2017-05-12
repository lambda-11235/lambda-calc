{-|
Module: Parse

The module to parse lisp expressions.
-}

module Parser where

import AST
import Lexer

import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim


type Parser = Parsec [LexOut] ()


match :: Token -> Parser ()
match tok = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' x = if x == tok then Just () else Nothing

var :: Parser String
var = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' (LVar name) = Just name
    match' _ = Nothing

pos :: (SourcePos -> LexOut -> [LexOut] -> SourcePos)
pos oldPos (LexOut _ line col _) _ = newPos (sourceName oldPos) line col


parser :: Parser AST
parser = expr <* eof


expr = do e <- nonapply
          es <- many nonapply
          return (foldl Apply e es)

nonapply = lambda <|> grouped <|> (fmap Var var)

lambda = do match LLambda
            arg <- var
            match LDot
            body <- nonapply
            return (Lambda arg body)

grouped = match LLParen *> expr <* match LRParen
