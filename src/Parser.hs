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

bindings :: Parser [(String, Expr)]
bindings = many bind


toplevel = (fmap (uncurry Bind)) bind <|> (fmap Expr expr)

bind = do match LLet
          name <- var
          match LEqual
          e <- expr
          return (name, e)


expr :: Parser Expr
expr = lambda <|> apply

lambda = do match LLambda
            args <- many1 var
            match LDot
            body <- expr
            return (lambda' args body)
  where
    lambda' [] body = body
    lambda' (var:vars) body = Lambda var (lambda' vars body)

apply = do e <- explicit
           es <- many explicit
           return (foldl Apply e es)

explicit = (fmap (\v -> Var v 0) var) <|> (between (match LLParen) (match LRParen) expr)
