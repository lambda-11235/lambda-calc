
module Eval ( eval
            ) where

import AST


increaseDepth' :: Nat -> Expr -> Expr
increaseDepth' depth (Var var idx) =
  if idx < depth then Var var idx else Var var (idx + 1)
increaseDepth' depth (Lambda var body) =
  Lambda var (increaseDepth' (depth + 1) body)
increaseDepth' depth (Apply x y) =
  Apply (increaseDepth' depth x) (increaseDepth' depth y)

increaseDepth :: Expr -> Expr
increaseDepth = increaseDepth' 0


subst' :: Nat -> Expr -> Expr -> Expr
subst' depth (Var var idx) x =
  if idx < depth then Var var idx
  else if idx == depth then x
  else Var var (idx - 1)
subst' depth (Lambda var body) x =
  Lambda var (subst' (depth + 1) body (increaseDepth x))
subst' depth (Apply y z) x = Apply (subst' depth y x) (subst' depth z x)

subst :: Expr -> Expr -> Expr
subst l x = subst' 0 l x


-- | Evaluates an expression to normal form or through a certain number of
-- applications.
eval :: Nat -> Expr -> Expr
eval _ v@(Var _ _) = v
eval n (Lambda var body) = Lambda var (eval n body)
eval n ap@(Apply x y) =
  if n <= 0 then ap else
    case eval n x of
      Lambda _ body -> eval (n - 1) (subst body (eval n y))
      expr -> Apply expr (eval n y)
