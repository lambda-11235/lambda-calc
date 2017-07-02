
module Main where

import AST
import Eval
import Lexer
import Parser

import Data.List (findIndex)
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
                                    putStrLn (ppExpr expr'')
            repl binds


ppExpr :: Expr -> String
ppExpr = ppExpr' False []

ppExpr' expParen vars (Lambda var body) =
  maybeParen expParen ("λ" ++ var ++ ". " ++ ppExpr' False (var:vars) body)
ppExpr' expParen vars (Apply e1 e2) =
  maybeParen expParen (ppExpr' True vars e1 ++ " " ++ ppExpr' True vars e2)
ppExpr' _ vars (Var var idx) =
  if maybe 0 fromIntegral (findIndex (== var) vars) == idx
  then var
  else var ++ "[" ++ show idx ++ "]"

maybeParen False s = s
maybeParen True s = "(" ++ s ++ ")"
