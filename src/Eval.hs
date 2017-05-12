
module Eval ( Error (..)
            , eval
            ) where

import AST

import Control.Monad.State
import qualified Data.Map as M


data Error = VarNotInScope String deriving (Eq, Show)

type LambdaState = StateT Env (Either Error)


raise :: Error -> LambdaState a
raise = lift . Left


eval :: AST -> LambdaState Function
eval (Lambda var body) = do env <- get
                            return (Function env var body)
eval (Apply e1 e2) = do f1 <- eval e1
                        f2 <- eval e2
                        apply f1 f2
eval (Var var) = do env <- get
                    case M.lookup var env of
                      Just f -> return f
                      Nothing -> raise (VarNotInScope var)


apply :: Function -> Function -> LambdaState Function
apply (Function closure var body) f =
  do oldEnv <- get
     put (M.insert var f closure)
     ret <- eval body
     put oldEnv
     return ret
