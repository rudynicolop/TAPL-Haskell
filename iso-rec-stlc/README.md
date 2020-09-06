# Iso-recursive Types Extending the Simply-Typed Lambda Calculus

### Syntax

Let the metavariables `x,y,z` range over expression variables, `e` range over expressions, `X,Y,Z` range over type variables, and `t,u,v` range over types.

```
t ::= unit | u -> v | X | mu X. t

e ::= () | x | fn x : t => e | e1 e2
  | fold [t] e | unfold [t] e
```

### Static Semantics

```
----------------[T-Unit]
 G |- () : unit

  G x = t
------------[T-Var]
 G |- x : t

        G[x:u] |- e : v
-----------------------------[T-Fun]
 G |- fn x : t => e : u -> v

 G |- e1 : u -> v   G |- e2 : u
--------------------------------[T-App]
          G |- e1 e2 : v

       G |- e : t{mu X. t/X}
---------------------------------[T-Fold]
 G |- fold [mu X. t] e : mu X. t

             G |- e : mu X. t
----------------------------------------[T-Unfold]
 G |- unfold [mu X. t] e : t{mu X. t/X}
```

### Dynamic Semantics

```
---------------------------------[E-Redux]
 (fn x : t => e1) e2 -> e1[e2/x]

    e1 -> e1'
-----------------[E-App]
 e1 e2 -> e1' e2

------------------------------------------[E-Annihilate]
 unfold [mu X. t] (fold [mu X. t] e) -> e

                e -> e'
---------------------------------------[E-Fold]
 fold [mu X. t] e -> fold [mu X. t] e'

                  e -> e'
-------------------------------------------[E-Unfold]
 unfold [mu X. t] e -> unfold [mu X. t] e'
```
