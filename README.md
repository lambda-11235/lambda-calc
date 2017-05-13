
# Lambda Calculus

A simple implementation of the untyped lambda calculus. This implementation uses
lexically scoped variables as well lazy evaluation. My primary motivation for
doing this project is to explore the implementation details of these two
language features. The interpreter is implemented as a REPL that allows
declarations to be made at the top level.

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
(λx. x)
λ> bot
<loops forever>
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
