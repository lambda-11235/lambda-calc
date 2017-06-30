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


parser :: Parser TopLevel
parser = toplevel <* eof


-- TODO: Remove try.
toplevel = try bind <|> (fmap Expr expr)

bind = do name <- var
          match LEqual
          e <- expr
          return (Bind name e)


expr = parened <|> (fmap (\s -> Var s 0) var)

-- NOTE: This is necessary to avoid backtracking.
parened = do match LLParen
             lambda <|> apply

lambda = do match LLambda
            args <- many1 var
            match LDot
            body <- expr
            match LRParen
            return (lambda' args body)
  where
    lambda' [] body = body
    lambda' (var:vars) body = Lambda var (lambda' vars body)

apply = do e <- expr
           es <- many1 expr
           match LRParen
           return (foldl Apply e es)
