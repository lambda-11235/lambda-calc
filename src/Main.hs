
module Main where

import AST
import Eval
import Lexer
import Parser

import qualified Data.Map as M
import qualified System.Console.Readline as RL
import Text.Parsec.Prim


-- | The maximum number of function applications allowed.
maxApps :: Nat
maxApps = 1000


main :: IO ()
main = repl M.empty

repl :: Bindings -> IO ()
repl binds =
  do input <- RL.readline "λ> "
     case input of
       Nothing -> return ()
       Just str ->
         do RL.addHistory str
            case runParser parser () "REPL" (scan str) of
              Left err -> print err

              Right (Bind name expr) ->
                case instantiateVars expr binds of
                  Left var ->
                    putStrLn ("Error: " ++ var ++ " not instantiated.")

                  Right expr -> repl (M.insert name expr binds)

              Right (Expr expr) ->
                case instantiateVars expr binds of
                  Left var ->
                    putStrLn ("Error: " ++ var ++ " not instantiated.")

                  Right expr' -> do let expr'' = eval maxApps expr'
                                    putStrLn (prettyPrintExpr expr'')
            repl binds


prettyPrintExpr (Lambda var body) = "(λ" ++ var ++ ". " ++ prettyPrintExpr body ++ ")"
prettyPrintExpr (Apply e1 e2) = "(" ++ prettyPrintExpr e1 ++ " " ++ prettyPrintExpr e2 ++ ")"
prettyPrintExpr (Var var idx) = var ++ "[" ++ show idx ++ "]"
