# Build and Run

To run a `.stlc` program, do:
```bash
cabal new-run stlc -- tests/simple.stlc
```

# Syntax

This language is composed of types, `t,a,b` patterns `p`, and expressions `e`. Let `x,y,z` range over variables, and `v` range over values.
```
t,a,b ::= u | a -> b | a * b | a + b

p ::= _ | x | () | p1, p2
  | Left a b p | Right a b p | p1 || p2

e ::= x | () | fun p : t => e | e1 e2
  | let p = e1 in e2 | e1, e2 | fst e | snd e
  | Left a b e | Right a b e
  | match e with p1 => e1 | ... | pn => en

v ::= () | fun p : t => e | v1, v2 | Left a b v | Right a b v
```
`u` is the unit type inhabited by `()`. `||` is the or-pattern.

# Static Semantics

Let `G` be the typing environment.

```
  G x = t
------------
 G |- x : t

-------------
 G |- () : u

 G |- p : a -| G'   p exhausts a   G' |- e : b
-----------------------------------------------
          G |- fun p : a => e : a -> b

 G |- e1 : a -> b   G |- e2 : a
--------------------------------
        G |- e1 e2 : b

 G |- e1 : a   G |- p : a -| G'   p exhausts a   G' |- e : b
-------------------------------------------------------------
                G |- let p = e1 in e2 : b  

 G |- e1 : a   G |- e2 : b
---------------------------
    G |- e1, e2 : a * b

 G |- e : a * b
----------------
 G |- fst e : a

 G |- e : a * b
----------------
 G |- snd e : b     

       G |- e : a
-------------------------
 G |- Left a b e : a + b

       G |- e : b
--------------------------
 G |- Right a b e : a + b


 G |- e : a   G |- pi : a -| Gi   pi exhaust a   Gi |- ei : b
--------------------------------------------------------------
       G |- match e with p1 => e1 | ... | pn => en : b

-----------------
 G |- _ : t -| G

--------------------------
 G |- x : t -> (x:t :: G)

------------------
 G |- () : u -| G

 G |- p1 : a -| G1   G |- p2 : b -| G2
    (FV p1) and (FV p2) are disjoint
---------------------------------------
 G |- p1, p2 : a * b -| union G1 G2

       G |- p : a -| G'
-------------------------------
 G |- Left a b p : a + b -| G'

        G |- p : b -| G'
--------------------------------
 G |- Right a b p : a + b -| G'


 G |- p1 : t -| G'   G |- p2 : t -| G'
---------------------------------------
        G |- p1 || p2 : t -| G'
```

`p exhausts t` denotes that pattern `p`, or a set of patterns, is exhaustive with respect to type `t`. I have implemented the algorithm as described in [this Inria article](http://moscova.inria.fr/~maranget/papers/warn/index.html).

# Dynamic Semantics

This language has eager/strict evaluation to simplify pattern-matching semantics.

Let `e[v/p]` denote the substitution of sub-values of `v` for variables embedded in pattern `p` in expression `e`.

```
------------------------------
 (fun p : t => e) v -> e[v/p]

                   e2 -> e2'
-----------------------------------------------
 (fun p : t => e1) e2 -> (fun p : t => e1) e2'

    e1 -> e1
-----------------
 e1 e2 -> e1' e2

--------------------------
 let p = v in e -> e[v/p]

               e1 -> e1'
---------------------------------------
 let p = e1 in e2 -> let p = e1' in e2

    e -> e'
---------------
 v, e -> v, e'

     e1 -> e1'
-------------------
 e1, e2 -> e1, e2'

--------------------
 fst (v1, v2) -> v1

      e -> e'
 -----------------
  fst e -> fst e'

--------------------
  snd (v1, v2) -> v2

     e -> e'
-----------------
 snd e -> snd e'

          e -> e'
---------------------------
 Left a b e -> Left a b e'

           e -> e'
-----------------------------
 Right a b e -> Right a b e'

          pi filters v
-----------------------------------
 match v with pi => ei -> ei[v/pi]

                     e -> e'
-------------------------------------------------
 match e with pi => ei -> match e' with pi => ei
```
`pi filters v` is [defined within this paper](http://moscova.inria.fr/~maranget/papers/warn/warn003.html).

# Idiosyncrasies

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
