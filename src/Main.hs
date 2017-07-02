
module Main where

import AST
import Eval
import Lexer
import Parser

import Control.Exception
import Data.Foldable (foldlM, foldrM)
import Data.List (findIndex)
import qualified Data.Map as M
import qualified System.Console.Readline as RL
import System.Environment (getArgs)
import System.Exit
import Text.Parsec.Prim


-- | The maximum number of function applications allowed.
maxApps :: Nat
maxApps = 1000


main :: IO ()
main = do files <- getArgs
          binds <- loadFiles files
          seq binds (repl binds)

repl :: Bindings -> IO a
repl binds =
  do input <- RL.readline "λ> "
     case input of
       Nothing -> exitSuccess
       Just str ->
         do RL.addHistory str
            toks <- catch (evaluate (scan str)) (\e -> print (e :: ErrorCall) >> repl binds)
            if null toks then repl binds else
              case runParser parser () "REPL" toks of
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


loadFiles :: [String] -> IO Bindings
loadFiles = foldlM loadFile M.empty

loadFile :: Bindings -> String -> IO Bindings
loadFile binds file = do contents <- readFile file
                         case runParser bindings () file (scan contents) of
                           Left err -> error (show err)
                           Right bs -> return (foldl add' binds bs)
  where
    add' binds (name, expr) =
      case instantiateVars expr binds of
        Left var -> error ("Error (" ++ file ++ "): " ++ var ++ " not instantiated.")

        Right expr' -> M.insert name expr' binds


ppExpr :: Expr -> String
ppExpr = ppExpr' False []

ppExpr' expParen vars (Lambda var body) =
  maybeParen expParen ("λ" ++ var ++ ". " ++ ppExpr' False (var:vars) body)
ppExpr' expParen vars (Apply e1 e2) =
  maybeParen expParen (ppExpr' True vars e1 ++ " " ++ ppExpr' True vars e2)
ppExpr' _ vars (Var var idx) =
  let debruijn = var ++ "[" ++ show idx ++ "]" in
    case findIndex (== var) vars of
      Nothing -> debruijn
      Just idx' ->
        if fromIntegral idx' == idx
         then var
         else debruijn

maybeParen False s = s
maybeParen True s = "(" ++ s ++ ")"
