
module Eval ( Error (..)
            , eval
            ) where

import AST

import qualified Data.Map as M


data Error = VarNotInScope String deriving (Eq, Show)


{-
   When evaluating a lambda expression we package the new function with the
   current environment, thus creating a closure. When applying an expression we
   must first evaluate it and then apply it to and unevaluated chunk containing
   it argument. When looking up a variable we must evaluate the chunk that it is
   bond to.
-}
eval :: Expr -> Env -> Either Error Function
eval (Lambda var body) env = Right (Function env var body)
eval (Apply e1 e2) env = do f <- eval e1 env
                            apply f (Chunk env e2)
eval (Var var) env = case M.lookup var env of
                       Just (Chunk env' expr) -> eval expr env'
                       Nothing -> Left (VarNotInScope var)


{-
   Application basically consists of evaluating the body of a function with its
   closure as the environment with the addition of the argument being bond to
   its variable.
-}
apply :: Function -> Chunk -> Either Error Function
apply (Function closure var body) chunk = eval body (M.insert var chunk closure)
