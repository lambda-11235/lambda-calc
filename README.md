
# Lambda Calculus

A simple implementation of the untyped lambda calculus. This implementation uses
lexically scoped variables as well lazy evaluation.

## TODO

- Fix EBNF grammer.
- Write better tests.

## Syntax

Note that when looking at the EBNF below `λx. x x` will parse to `(λx. x) x`
instead of `λx. (x x)`. This shold be fixed.

### EBNF

```
expr = nonapply , { nonapply } ;  (* Application is left associative (ie. x y z = (x y) z). *)

nonapply = lambda , var , '.' , nonapply
         | '(' , expr , ')'
         | var ;

var = char , { char } ;

lambda = '\' | 'λ' ;
char = ? [a-zA-Z] ? ;
```

## Example

```
λ> (\x. \y. x) (\x. x) ((\x. (x x)) (\x. (x x)))
λx. x
λ> (\x. \y. x)
λx. λy. x
λ> (\x. (x x)) (\x. (x x))
<loops forever>
```
