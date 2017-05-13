
module Check ( Error (..)
             , check
             ) where

import AST

import qualified Data.Set as S
import qualified Data.Map as M


data Error = UninstatiatedVar String deriving (Eq, Show)


-- | Statically checks for semantic errors.
check :: Expr -> Env -> Either Error ()
check expr env = check'
  where
    check' = do checkVars expr (S.fromList (M.keys env))


-- | Statically checks that all variables are instantiated by a lambda
-- definiton.
checkVars :: Expr -> S.Set String -> Either Error ()
checkVars (Lambda var body) vars = checkVars body (S.insert var vars)
checkVars (Apply e1 e2) vars = do checkVars e1 vars
                                  checkVars e2 vars
checkVars (Var var) vars = if S.member var vars
                             then Right ()
                             else Left (UninstatiatedVar var)

