
# Lambda Calculus

A simple implementation of the untyped lambda calculus. It is implemented as an
interpreter. The interpreter evaluates expressions to either normal form, or
through a certain number of function applications, whichever is shorter.

## Syntax

### EBNF

```
expr = '(' , lambda , var , { var } , '.' , expr , ')'
     | '(' , expr , expr ')'
     | var ;

var = char , { char } ;

lambda = '\' | 'λ' ;
char = ? [a-zA-Z] ? ;
```

Additionally, the top level entries in the REPL have the syntax
```
toplevel = var , '=' , expr
         | expr ;
```

## Example Session

```
> stack setup
...
> stack build
...
> stack exec typed-lambda-calc
λ> id = (\x. x)
λ> const = (\x y. x)
λ> bot = ((\x. (x x)) (\x. (x x)))
λ> ((const id) bot)
(λx. x[0])
λ> bot
((λx. (x[0] x[0])) (λx. (x[0] x[0])))
```

One should note that `bot` should never return if we fully evaluated the term.
This demonstrates that the interpreter will give up evaluation after a certain
number of function applications. The bracketed numbers (ie. the 0 in `x[0]`)
represent the de Bruijn indices of the variables, and are used to disambiguate
potentially ambiguous variable names caused by lambda capture. The canonical
example being
```
λ> (\x. ((\f x. (f x)) x))
(λx. (λx. (x[1] x[0])))
```

Note that declarations of the form `v = e` approximately translate into
`((\v. <Rest of REPL session>) e)`. Here's an example
```
λ> id = (\x. x)
λ> id = id
λ> id
```
approximately translates to `((\id. ((\id. id) id)) (\x. x))`.

## TODO

- Write better tests.
