
module Main where

import AST
import Eval
import Lexer
import Parser

import System.IO
import Text.Parsec.Prim


main :: IO ()
main = do putStr "λ> "
          hFlush stdout
          str <- getLine
          case runParser parser () "REPL" (scan str) of
            Left err -> print err
            Right ast -> case eval ast of
                           Right f -> putStrLn (prettyPrintFunc f)
                           Left (VarNotInScope var) ->
                             putStrLn ("Error: " ++ var ++ " not in scope.")
          main
