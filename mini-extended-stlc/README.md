# Build and Run

To run an STLC program, do:
```bash
cabal new-run stlc -- tests/simple.stlc
```

## Common Parsing Errors

Sadly the parser gives pretty poor error messages so here are a few common ones:

#### Type Arguments

`Left` and `Right` require type arguments.
```
Left t1 t2 e
Right t1 t2 e
```

#### Forgetting `in` in `let`-Expressions

'let'-expressions must be in closed form:
```
let p = e1 in e2
```

#### Using `_` in an Expression

The wild-card `_` is strictly for patterns.

#### Match-Expression Delimiter

The parser only allows the `|` to go between cases in a `match`-expression:
```
match e with
p1 => e1
| p2 => e2
| p3 => e3
...
| pn => en
end
```
