
# Lambda Calculus

A simple implementation of the untyped lambda calculus.

## TODO

- Implement a parser and REPL.

## Syntax

```
term = '(' , lambda , var , '.' , term , ')'
     | '(' , term , term , ')'
     | var ;

var = char , { char } ;

lambda = '\' | 'λ' ;
char = ? [a-zA-Z] ? ;
```

## Example

Without a parser the AST must be entered directly

```
> ghci src/Eval.hs src/AST.hs

GHCi> evalStateT (eval (Apply (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x")))) M.empty

Right (Function {getClosure = fromList [("x",Function {getClosure = fromList [], getVar = "x", getBody = Var "x"})], getVar = "y", getBody = Var "x"})

it :: Either Error Function
```
