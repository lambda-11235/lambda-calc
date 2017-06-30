
module AST where

import Data.List (findIndex)
import qualified Data.Map as M
import Data.Word

type Nat = Word64
type Bindings = M.Map String Expr


data Expr = Lambda String Expr
          | Apply Expr Expr
          | Var String Nat
           deriving (Eq, Show)


data TopLevel = Bind String Expr
              | Expr Expr
              deriving (Eq, Show)



-- | Instantiates all de Bruijn indices and replaces bindings with their
-- corresponding expressions. When an unbound variable is found its name is
-- returned.
instantiateVars :: Expr -> Bindings -> Either String Expr
instantiateVars expr binds = instantiateVars' expr [] binds

instantiateVars' :: Expr -> [String] -> Bindings -> Either String Expr
instantiateVars' (Lambda var body) vars binds =
  Lambda var <$> (instantiateVars' body (var:vars) binds)
instantiateVars' (Apply x y) vars binds =
  Apply <$> (instantiateVars' x vars binds) <*> (instantiateVars' y vars binds)
instantiateVars' (Var var _) vars binds =
  case findIndex (== var) vars of
    Nothing ->
      case M.lookup var binds of
        Nothing -> Left var
        Just expr -> Right expr
    Just idx -> Right (Var var (fromIntegral idx))
