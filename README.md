
# Lambda Calculus

A simple implementation of the untyped lambda calculus. This implementation uses
lexically scoped variables as well lazy evaluation.

## TODO

- Fix EBNF grammer.
- Add readline functionality.
- Write better tests.

## Syntax

This EBNF should be changed to allow expressions like `\x y z. x y z`, where
application is left associative and binds more tightly than abstraction. This
expression should thus be equivilent to what is now `(\x y z. ((x y) z))`.

### EBNF

```
expr = '(' , lambda , var , { var } , '.' , expr , ')'
     | '(' , expr , expr ')'
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
