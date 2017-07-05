
module Eval ( eval
            ) where

import AST (Nat)
import qualified AST as A

import Data.STRef
import Control.Monad.ST


data Chunk s = Code (STRef s (Either (Repr s) (Repr s)))
             | AddIndex Nat (STRef s (Either (Chunk s) (Repr s)))
             | Subst Nat (STRef s (Either (Chunk s) (Repr s))) (Chunk s)

data Repr s = Var String Nat
            | Lambda String (Chunk s)
            | Apply (Chunk s) (Chunk s)



fromUneval :: Repr s -> ST s (Chunk s)
fromUneval term = do ref <- newSTRef (Left term)
                     return (Code ref)

fromEval :: Repr s -> ST s (Chunk s)
fromEval term = do ref <- newSTRef (Right term)
                   return (Code ref)

fromAddIndex :: Nat -> Chunk s -> ST s (Chunk s)
fromAddIndex depth chunk = do ref <- newSTRef (Left chunk)
                              return (AddIndex depth ref)

fromSubst :: Nat -> Chunk s -> Chunk s -> ST s (Chunk s)
fromSubst depth chunk x = do ref <- newSTRef (Left chunk)
                             return (Subst depth ref x)

force :: Chunk s -> ST s (Repr s)
force (Code ref) =
  do code <- readSTRef ref
     case code of
       Left term ->
         do term' <- eval' term
            writeSTRef ref (Right term')
            return term'

       Right term -> return term
force (AddIndex depth ref) =
  do code <- readSTRef ref
     case code of
       Left chunk ->
         do term <- force chunk
            term' <- addIndex depth term
            writeSTRef ref (Right term')
            return term'

       Right term -> return term
force (Subst depth ref x) =
  do code <- readSTRef ref
     case code of
       Left chunk ->
         do term <- force chunk
            chunk' <- subst depth term x
            term' <- force chunk'
            writeSTRef ref (Right term')
            return term'

       Right term -> return term



addIndex :: Nat -> Repr s -> ST s (Repr s)
addIndex depth (Var var idx) = return $
  if idx < depth then Var var idx else Var var (idx + 1)
addIndex depth (Lambda var body) =
  Lambda var <$> (fromAddIndex (depth + 1) body)
addIndex depth (Apply e1 e2) =
  Apply <$> (fromAddIndex depth e1) <*> (fromAddIndex depth e2)


subst :: Nat -> Repr s -> Chunk s -> ST s (Chunk s)
subst depth v@(Var var idx) x =
  if idx < depth then fromEval v
  else if idx == depth then return x
  else fromEval (Var var (idx - 1))
subst depth (Lambda var body) x =
  do x' <- fromAddIndex 0 x
     l <- Lambda var <$> (fromSubst (depth + 1) body x')
     fromUneval l
subst depth (Apply e1 e2) x =
  do ap <- Apply <$> (fromSubst depth e1 x) <*> (fromSubst depth e2 x)
     fromUneval ap


eval' :: Repr s -> ST s (Repr s)
eval' ap@(Apply fst snd) =
  do fst' <- force fst
     case fst' of
       Lambda _ body -> do chunk <- fromSubst 0 body snd
                           force chunk
       _ -> return ap
eval' term = return term



fromAST :: A.Expr -> ST s (Repr s)
fromAST (A.Var var idx) = return (Var var idx)
fromAST (A.Lambda var body) =
  do body' <- fromAST body
     Lambda var <$> (fromUneval body')
fromAST (A.Apply e1 e2) =
  do e1' <- fromAST e1
     e2' <- fromAST e2
     Apply <$> (fromUneval e1') <*> (fromUneval e2')

toAST :: Repr s -> ST s A.Expr
toAST (Var var idx) = return (A.Var var idx)
toAST (Lambda var body) =
  do body' <- force body
     A.Lambda var <$> (toAST body')
toAST (Apply e1 e2) =
  do e1' <- force e1
     e2' <- force e2
     A.Apply <$> (toAST e1') <*> (toAST e2')

eval :: A.Expr -> A.Expr
eval term = runST $ do term' <- fromAST term
                       term'' <- eval' term'
                       toAST term''
