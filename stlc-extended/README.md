# The Extended Simply-Typed Lambda Calculus

## Syntax

This language features product, record, and sum types, as well as pattern matching. All instances of sum types are required to be labeled with a type or type synonym to ensure the uniqueness of types.

Base types implicitly are the empty tuple type `()` i.e. the unit type, the empty record type `{}`, and a sum type where at least one of the variants is just a label.
```
t ::= X
    | t1 -> t2
    | t1 * t2 * ... * tn
    | {l1:t1; l2:t2; ...; ln:tn}
    | [l1:t1; l2:t2; ...; ln:tn]

p ::= x
    | p1 * p2 * ... * pn
    | {l1=p1; l2=p2; ...; ln=pn}
    | t.li pi

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

## Static Semantics

Expressions are checked under the judgment `D,G |- e : t`.
```
  G x = t
--------------
 D,G |- x : t

 D |- t ok   D,G |- p : t -| G'   D,G' |- e : t'
-------------------------------------------------
        D,G |- fun p : t => e : t -> t'

 D,G |- e1 : t -> t'   D,G |- e2 : t
--------------------------------------
        D,G |- e1 e2 : t'

 D,G |- e1 : t1   D,G |- p : t1 -| G'   D,G' |- e2 : t2
---------------------------------------------------------
            D,G |- let p = e1 in e2 : t2

                D,G |- ei : ti
----------------------------------------------
 D,G |- (e1, e2, ..., en) : (t1, t2, ..., tn)

 D,G |- e : (t1, ..., ti, ..., tn)
-----------------------------------
        D,G |- e.i : ti

                         D,G |- ei : ti
----------------------------------------------------------------
 D,G |- {l1=e1; l2=e2; ...; ln=en} : {l1:t1; l2:t2; ...; ln:tn}

 D,G |- e : {l1:t1; ...; li:ti; ...; ln:tn}
--------------------------------------------
              D,G |- e.li : ti

 D |- t ok   D ~> t : [l1:t1; ...; li:ti; ...; ln:tn]   D,G |- e : ti
----------------------------------------------------------------------
                        G |- t.li e : t

   D,G |- e : t   D,G |- pi : t -| G'   D,G' |- ei : t'
----------------------------------------------------------
 D,G |- match (e as t) with ... `|` pi -> ei ... end : t'

    D[X->t],G |- e : t'
-----------------------------
 D,G |- type X = t in e : t'
```
The match-case is degenerate: it needs to check whether all possible patterns are checked.

Types/type-synonyms are confirmed to be well-defined under the judgment `D |- t ok`.
```
  D X = t
-----------
 D |- X ok

 D |- t1 ok   D |- t2 ok
-------------------------
     D |- t1 -> t2 ok

        D |- ti ok
----------------------------
 D |- t1 * t2 * ... * tn ok

            D |- ti ok
------------------------------------
 D |- {l1:t1; l2:t2; ...; ln:tn} ok

            D |- ti ok
------------------------------------
 D |- [l1:t1; l2:t2; ...; ln:tn] ok
```

Types/type-synonyms are resolved under the judgment `D ~> t : t'`.
```
 D X = t   D ~> t : t'
-----------------------
      D ~> X : t'

 D ~> t1 : t1'   D ~> t2 : t2'
-------------------------------
  D ~> t1 -> t2 : t1' -> t2'

                D ~> ti : ti'
------------------------------------------------
 D ~> t1 * t2 * ... * tn : t1' * t2' * ... * tn'

                  D ~> ti : ti'
--------------------------------------------------
 D ~> {l1=t1; ...; ln=tn} : {l1=t1'; ...; ln=tn'}

                  D ~> ti : ti'
--------------------------------------------------
 D ~> [l1=t1; ...; ln=tn] : [l1=t1'; ...; ln=tn']
```
Consider that the rule for type-synonyms may pose problems: it may never be able to resolve a type name.

This should not occur in typing programs. Consider the pathological case:
```
type A = B in type B = A in ()
```
After the second synonym declaration, hypothetically `D` maps `A` to `B` and `B` to `A`, creating a loop. However, note that the first declaration fails to type-check because the name `B` is unbound.

Patterns are checked under the judgment `D,G |- p : t -| G'`. Pattern typing necessitates adding new bindings to `G`.
```
-------------------------
 D,G |- x : t -| G'[x:t]

 FV(pi) mutually disjoint  D,G_(i-1) |- pi : ti -| G_i
-------------------------------------------------------
    D,G |- p1 * ... * pn : t1 * t2 * ... * tn -| G_n

  FV(pi) mutually disjoint   D,G_(i-1) |- pi : ti -| G_i
----------------------------------------------------------
  D,G |- {l1=p1; ...; ln=pn} : {l1=t1; ...; ln=tn} -| G_n

 D |- t ok   [l1:t1; ...; li:ti; ...; ln:tn]   D,G |- pi : ti -| G'
--------------------------------------------------------------------
                    D,G |- t.li pi : t -| G'
```
