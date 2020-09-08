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
------------[T-Var]
 G |- x : t

-------------[T-Unit]
 G |- () : u

 G |- p : a -| G'   p exhausts a   G' |- e : b
-----------------------------------------------[T-Fun]
          G |- fun p : a => e : a -> b

 G |- e1 : a -> b   G |- e2 : a
--------------------------------[T-App]
        G |- e1 e2 : b

 G |- e1 : a   G |- p : a -| G'   p exhausts a   G' |- e : b
-------------------------------------------------------------[T-Let]
                G |- let p = e1 in e2 : b  

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
-------------------------[T-Left]
 G |- Left a b e : a + b

       G |- e : b
--------------------------[T-Right]
 G |- Right a b e : a + b


 G |- e : a   G |- pi : a -| Gi   pi exhaust a   Gi |- ei : b
--------------------------------------------------------------[T-Match]
       G |- match e with p1 => e1 | ... | pn => en : b

-----------------[T-Pattern-Wild]
 G |- _ : t -| G

--------------------------[T-Pattern-Var]
 G |- x : t -> (x:t :: G)

------------------[T-Pattern-Unit]
 G |- () : u -| G

 G |- p1 : a -| G1   G |- p2 : b -| G2
    (FV p1) and (FV p2) are disjoint
---------------------------------------[T-Pattern-Pair]
 G |- p1, p2 : a * b -| union G1 G2

       G |- p : a -| G'
-------------------------------[T-Pattern-Left]
 G |- Left a b p : a + b -| G'

        G |- p : b -| G'
--------------------------------[T-Pattern-Right]
 G |- Right a b p : a + b -| G'


 G |- p1 : t -| G'   G |- p2 : t -| G'
---------------------------------------[T-Pattern-Or]
        G |- p1 || p2 : t -| G'
```

`p exhausts t` denotes that pattern `p`, or a set of patterns, is exhaustive with respect to type `t`. I have implemented the algorithm as described in [this Inria article](http://moscova.inria.fr/~maranget/papers/warn/index.html).

# Dynamic Semantics

This language has eager/strict evaluation to simplify pattern-matching semantics.

Let `e[v/p]` denote the substitution of sub-values of `v` for variables embedded in pattern `p` in expression `e`.

```
------------------------------[E-Redux]
 (fun p : t => e) v -> e[v/p]

                   e2 -> e2'
-----------------------------------------------[E-App-1]
 (fun p : t => e1) e2 -> (fun p : t => e1) e2'

    e1 -> e1
-----------------[E-App-2]
 e1 e2 -> e1' e2

--------------------------[E-Let-1]
 let p = v in e -> e[v/p]

               e1 -> e1'
---------------------------------------[E-Let-2]
 let p = e1 in e2 -> let p = e1' in e2

    e -> e'
---------------[E-Pair-1]
 v, e -> v, e'

     e1 -> e1'
-------------------[E-Pair-2]
 e1, e2 -> e1, e2'

--------------------[E-Fst-1]
 fst (v1, v2) -> v1

      e -> e'
 -----------------[E-Fst-2]
  fst e -> fst e'

---------------------[E-Snd-1]
  snd (v1, v2) -> v2

     e -> e'
-----------------[E-Snd-2]
 snd e -> snd e'

          e -> e'
---------------------------[E-Left]
 Left a b e -> Left a b e'

           e -> e'
-----------------------------[E-Right]
 Right a b e -> Right a b e'

           pi filters v
-----------------------------------[E-Match-1]
 match v with pi => ei -> ei[v/pi]

                     e -> e'
-------------------------------------------------[E-Match-2]
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

# Known Issues

### Parsing Precedence
The parser does not correctly handle expression nesting on occasion. Until I fix this, for now wrapping sub-expressions in parentheses when bizarre type-checking issues arise seems to work.
