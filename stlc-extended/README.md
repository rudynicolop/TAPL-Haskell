# The Extended Simply-Typed Lambda Calculus

## Syntax

```
t ::= t1 -> t2
    | t1 * t2 * ... * tn
    | {l1:t1; l2:t2; ...; ln:tn}
    | <l1:t1; l2:t2; ...; ln:tn>

p ::= x
    | p1 * p2 * ... * pn
    | {l1=p1; l2=p2; ...; ln=pn}
    | t.l pi

e ::= x
    | fun p : t => e
    | e1 e2
    | let p = e1 in e2
    | (e1, e2, ..., en)
    | e.i
    | {l1=e1; l2=e2; ...; ln=en}
    | e.li
    | t.li e
    | match (e as t) with
      `|` p1 -> e1
      ...
      `|` pn -> en
      end
    | type X = t in e
```
