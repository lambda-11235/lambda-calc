
module AST where

import qualified Data.Map as M


type Env = M.Map String Chunk


data Chunk = Chunk { getEnv :: Env
                   , getExpr :: Expr }
                   deriving (Eq, Show)


data Function = Function { getClosure :: Env
                         , getVar :: String
                         , getBody :: Expr }
                         deriving (Eq, Show)


data Expr = Lambda String Expr
          | Apply Expr Expr
          | Var String
           deriving (Eq, Show)


data TopLevel = Bind String Expr
              | Expr Expr
              deriving (Eq, Show)


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
