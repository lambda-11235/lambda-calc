
module Check ( Error (..)
             , check
             ) where

import AST

import Control.Monad.State
import qualified Data.Set as S


data Error = UninstatiatedVar String deriving (Eq, Show)

type CheckState = StateT (S.Set String) (Either Error)


-- | Statically checks for semantic errors.
check :: AST -> Either Error ()
check ast = evalStateT check' S.empty
  where
    check' = do checkVars ast


-- | Statically checks that all variables are instantiated by a lambda
-- definiton.
checkVars :: AST -> CheckState ()
checkVars (Lambda var body) = do modify (S.insert var)
                                 checkVars body
checkVars (Apply e1 e2) = do checkVars e1
                             checkVars e2
checkVars (Var var) = do vars <- get
                         if S.member var vars
                            then return ()
                            else lift (Left (UninstatiatedVar var))

