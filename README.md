
# Lambda Calculus

A simple implementation of the untyped lambda calculus. It is implemented as an
interpreter. The interpreter evaluates expressions to either normal form, or
through a certain number of function applications, whichever is shorter.

## Syntax

### EBNF

```
expr = lambda , var , { var } , '.' , expr
     | explicit , { explicit } ;

explicit = var
         | '(' , expr , ')' ;

var = char , { char } ;

lambda = '\' | 'λ' ;
char = ? [a-zA-Z] ? ;
```

Additionally, the top level entries in the REPL have the syntax
```
toplevel = "let" , var , '=' , expr
         | expr ;
```

## Example Session

```
> stack setup
...
> stack build
...
> stack exec typed-lambda-calc
λ> let id = \x. x
λ> let const = \x y. x
λ> let bot = (\x. x x) (\x. x x)
λ> const id bot
λx. x
λ> bot
(λx. x x) (λx. x x)
```

One should note that `bot` should never return if we fully evaluated the term.
This demonstrates that the interpreter will give up evaluation after a certain
number of function applications.

The interpreter uses de Buijn indexes during evaluation. Thus, when lambda
capture occurs variable names aren't rewritten. To avoid confusen, when a
variable `v` doesn't refer to the tightest binding with the same name we add its
de Bruijn index `n` next to it using the notation `v[n]`. An example would be
```
λ> \x. (\f x. f x) x
λx. λx. x[1] x
```

Note that declarations of the form `v = e` approximately translate into
`(\v. <Rest of REPL session>) e`. Here's an example
```
λ> id = \x. x
λ> id = id
λ> id
```
approximately translates to `(\id. (\id. id) id) (\x. x)`.

## TODO

- Write better tests.
