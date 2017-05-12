
module Lazy where

import AST
import Eval

import qualified Data.Map as M


-- | Should return true if laziness is implemented correctly, otherwise it
-- returns bottom (enters an infinite loop).
lazyTest :: Bool
lazyTest = eval expr M.empty == Right (Function M.empty "x" (Var "x"))
  where
    expr = Apply (Apply constF idF) bot
    constF = Lambda "x" (Lambda "y" (Var "x"))
    idF = Lambda "x" (Var "x")
    bot = Apply omega omega
    omega = (Lambda "x" (Apply (Var "x") (Var "x")))
