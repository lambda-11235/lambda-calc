
# Lambda Calculus

A simple implementation of the untyped lambda calculus. It is implemented as an
interpreter. The interpreter evaluates expressions to normal form, if possible.

## Syntax

Comments begin with a `#` and extend to the end of the line.

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

## Semantics

The semantics are the same as for normal lambda calculus. This implementation
in particular uses lazy evaluation.

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
<loops forever>
```

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
