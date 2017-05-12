
module Eval ( Error (..)
            , eval
            ) where

import AST

import qualified Data.Map as M


data Error = VarNotInScope String deriving (Eq, Show)


eval :: AST -> Env -> Either Error Function
eval (Lambda var body) env = Right (Function env var body)
eval (Apply e1 e2) env = do f <- eval e1 env
                            apply f (Chunk env e2)
eval (Var var) env = case M.lookup var env of
                       Just (Chunk env' ast) -> eval ast env'
                       Nothing -> Left (VarNotInScope var)


apply :: Function -> Chunk -> Either Error Function
apply (Function closure var body) chunk = eval body (M.insert var chunk closure)
