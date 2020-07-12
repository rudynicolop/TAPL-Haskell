# The Extended Simply-Typed Lambda Calculus

## Syntax

```
t ::= t1 -> t2
    | t1 * t2 * ... * tn
    | {l1:t1; l2:t2; ...; ln:tn}
    | <l1:t1; l2:t2; ...; ln:tn> as X

e ::= x
    | fun x : t => e
    | e1 e2
    | let x = e1 in e2
    | (e1, e2, ..., en)
    | e.i
    | {l1=e1; l2=e2; ...; ln=en}
    | e.l
    | type X = t in e
    | li e as X
    | match (e as X) with
      `|` l1 x1 -> e1
      ...
      `|` ln xn -> en
      end
```
