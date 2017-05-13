
module AST where

import qualified Data.Map as M


-- | The current execution environment. This is a map between bond variables and
-- the unevaluated chunks they correspond to.
type Env = M.Map String Chunk


-- | A chunk is an unevaluated piece of code. We must store enough information
-- to fully evaluate the chunk, which includes the environment it was found in
-- as well as the AST of the code.
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
