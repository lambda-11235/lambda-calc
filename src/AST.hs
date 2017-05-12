
module AST where

import qualified Data.Map as M


type Env = M.Map String Chunk


data Chunk = Chunk { getEnv :: Env
                   , getAST :: AST }
                   deriving (Eq, Show)


data Function = Function { getClosure :: Env
                         , getVar :: String
                         , getBody :: AST }
                         deriving (Eq, Show)


data AST = Lambda String AST
         | Apply AST AST
         | Var String
         deriving (Eq, Show)


prettyPrintFunc :: Function -> String
prettyPrintFunc (Function _ var body) = prettyPrintAST (Lambda var body)

prettyPrintAST (Lambda var body) = "λ" ++ var ++ ". " ++ prettyPrintAST body
prettyPrintAST (Apply e1 e2) = "(" ++ prettyPrintAST e1 ++ ") (" ++ prettyPrintAST e2 ++ ")"
prettyPrintAST (Var var) = var
