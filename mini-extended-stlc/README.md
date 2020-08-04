# Build and Run

To run a `.stlc` program, do:
```bash
cabal new-run stlc -- tests/simple.stlc
```

# Syntax

This language is composed of types, `t,a,b` patterns `p`, and expressions `e`. Let `x,y,z` range over variables.
```
t,a,b ::= u | a -> b | a * b | a + b

p ::= _ | x | () | p1, p2
  | Left a b p | Right a b p | p1 || p2

e :: x | () | fun p : t => e | e1 e2
  | let p = e1 in e2 | e1, e2 | fst e | snd e
  | Left a b e | Right a b e
  | match e with p1 => e1 | ... | pn => en
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
```

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
