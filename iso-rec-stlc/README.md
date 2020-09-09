# Iso-recursive Types Extending the Simply-Typed Lambda Calculus

### Build and Run

To run a `.stlc` program enter:

```bash
cabal new-run irecstlc -- tests/unit.stlc
```

### Syntax

Let the metavariables `x,y,z` range over expression variables, `e` range over expressions, `v` range over values, `X,Y,Z` range over type variables, and `a,b,t` range over types.

```
t ::= unit | a -> b | a * b | a + b | X | mu X. t

e ::= () | x | fn x : t => e
   | e1 e2 | e1, e2 | fst e | snd e
   | Left t e | Right t e
   | let x = e1 in e2
   | case e of e1 | e2
   | fold [t] e | unfold [t] e

v ::= () | fn x : t => e | v1, v2
   | Left t v | Right t v | fold [t] v
```

### Static Semantics

```
----------------[T-Unit]
 G |- () : unit

  G x = t
------------[T-Var]
 G |- x : t

        G[x:a] |- e : b
-----------------------------[T-Fun]
 G |- fn x : a => e : a -> b

 G |- e1 : a -> b   G |- e2 : a
--------------------------------[T-App]
          G |- e1 e2 : b

 G |- e1 : a   G |- e2 : b
---------------------------[T-Pair]
     G |- e1, e2 : a * b

 G |- e : a * b
----------------[T-Fst]
 G |- fst e : a

 G |- e : a * b
----------------[T-Snd]
 G |- snd e : b

         G |- e : a
-----------------------[T-Left]
 G |- Left b e : a + b

          G |- e : b
------------------------[T-Right]
 G |- Right a e : a + b

 G |- e1 : a   G[x:a] |- e2 : b
--------------------------------[T-Let]
    G |- let x = e1 in e2 : b

 G |- e : a + b   G |- e1 : a -> t   G |- e2 : b -> t
------------------------------------------------------[T-Case]
              G |- case e of e1 | e2 : t

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

------------------------------[E-Let]
 let x = e1 in e2 -> e2[e1/x]

    e -> e'
---------------[E-Pair-1]
 v, e -> v, e'

     e1 -> e1'
-------------------[E-Pair-2]
 e1, e2 -> e1', e2

--------------------[E-Fst-1]
 fst (e1, e2) -> e1

     e -> e'
-----------------[E-Fst-2]
 fst e -> fst e'

--------------------[E-Snd-1]
 snd (e1, e2) -> e2

     e -> e'
-----------------[E-Snd-2]
 snd e -> snd e'

-----------------------[E-Left]
 Left t e -> Left t e'

-------------------------[E-Right]
 Right t e -> Right t e'

----------------------------------[E-Case-Left]
 case Left t e of e1 | e2 -> e1 e

-----------------------------------[E-Case-Right]
 case Right t e of e1 | e2 -> e1 e

                 e -> e'
-----------------------------------------[E-Case]
 case e of e1 | e2 -> case e' of e1 | e2

------------------------------------------[E-Annihilate]
 unfold [mu X. t] (fold [mu X. t] e) -> e

                e -> e'
---------------------------------------[E-Fold]
 fold [mu X. t] e -> fold [mu X. t] e'

                  e -> e'
-------------------------------------------[E-Unfold]
 unfold [mu X. t] e -> unfold [mu X. t] e'
```
