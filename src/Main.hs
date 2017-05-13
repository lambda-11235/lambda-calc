
module Main where

import AST
import Eval
import Lexer
import Parser

import qualified Data.Map as M
import System.IO
import Text.Parsec.Prim


main :: IO ()
main = repl M.empty

repl :: Env -> IO ()

repl env = do putStr "λ> "
              hFlush stdout
              str <- getLine
              case runParser parser () "REPL" (scan str) of
                Left err -> print err
                Right (Bind name expr) -> repl (M.insert name (Chunk env expr) env)
                Right (Expr expr) ->
                  case eval expr env of
                    Right f -> do putStr (prettyPrintClos f)
                                  putStrLn (prettyPrintFunc f)
                    Left (VarNotInScope var) ->
                      putStrLn ("Error: " ++ var ++ " not in scope.")
              repl env
