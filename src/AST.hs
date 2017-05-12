
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
