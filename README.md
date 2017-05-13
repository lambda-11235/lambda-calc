
# Lambda Calculus

A simple implementation of the untyped lambda calculus. This implementation uses
lexically scoped variables as well lazy evaluation.

## TODO

- Fix EBNF grammar.
- Add readline functionality.
- Write better tests.

## Syntax

This EBNF should be changed to allow expressions like `\x y z. x y z`, where
application is left associative and binds more tightly than abstraction. This
expression should thus be equivalent to what is now `(\x y z. ((x y) z))`.

### EBNF

```
expr = '(' , lambda , var , { var } , '.' , expr , ')'
     | '(' , expr , expr ')'
     | var ;

var = char , { char } ;

lambda = '\' | 'λ' ;
char = ? [a-zA-Z] ? ;
```

Additionally, the toplevel entries in the REPL have the syntax
```
toplevel = var , '=' , expr
         | expr ;
```

## Example

```
λ> id = (\x. x)
λ> const = (\x y. x)
λ> bot = ((\x. (x x)) (\x. (x x)))
λ> ((const id) bot)
(λx. x)
λ> bot
<loops forever>
```
