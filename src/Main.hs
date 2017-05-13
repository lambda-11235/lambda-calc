
module Main where

import AST
import Check
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
                Right (Bind name expr) ->
                  case check expr env of
                    Right () -> repl (M.insert name (Chunk env expr) env)
                    Left (UninstatiatedVar var) -> 
                      putStrLn ("Error: " ++ var ++ " not instantiated.")
                Right (Expr expr) ->
                  case eval expr env of
                    Right f -> do putStr (prettyPrintClos f)
                                  putStrLn (prettyPrintFunc f)
                    Left (VarNotInScope var) ->
                      putStrLn ("Error: " ++ var ++ " not in scope.")
              repl env


prettyPrintFunc :: Function -> String
prettyPrintFunc (Function _ var body) = prettyPrintExpr (Lambda var body)

prettyPrintClos :: Function -> String
prettyPrintClos (Function env _ _) = if M.null env
                                       then ""
                                       else "Closure Vars: " ++ ppClos (M.keys env) ++ "\n"
  where
    ppClos [] = ""
    ppClos [var] = var
    ppClos (var:vars) = var ++ " " ++ ppClos vars

prettyPrintExpr (Lambda var body) = "(λ" ++ var ++ ". " ++ prettyPrintExpr body ++ ")"
prettyPrintExpr (Apply e1 e2) = "(" ++ prettyPrintExpr e1 ++ " " ++ prettyPrintExpr e2 ++ ")"
prettyPrintExpr (Var var) = var
