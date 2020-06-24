# The Simply Typed Lambda Calculus

## Syntax

Let `e` range over expressions, `x` range over variables, and `t` range over types.

```
t ::= nat | bool | t1 -> t2 | (t)
n ::= Z | S n | 0 | 1 | 2 | 3 | ...
b ::= true | false
e ::= n | b | x | !e  | e + e
    | e - e | e * e | e = e
    | e < e | e & e | e `|` e |
    | if e1 then e2 else e3
    | fun x : t => e | e1 e2 | (e)
```

## Static Semantics

Typing judgments are of the form `G |- e : t`, where `G` maps variables to types.

```
--------------
 G |- n : nat

---------------
 G |- b : bool

  G x = t
------------
 G |- x : t

 G |- e1 : nat   G |- e2 : nat
-------------------------------
      G |- e1 + e2 : nat
...

 G |- e1 : bool   G |- e2 : t   G |- e3 : t  
---------------------------------------------
       G |- if e1 then e2 else e3 : t

       G[x:t] |- e : t'
-------------------------------
 G |- fun x : t => e : t -> t'

 G |- e1 : t -> t'   G |- e2 : t
---------------------------------
        G |- e1 e2 : t'
```

## Dynamic Semantics

The interpreter performs a lazy small-step evaluation.

```
 n1 + n2 = n3
---------------
 n1 + n2 -> n3

     e2 -> e2'
-------------------
 n + e2 -> n + e2'

      e1 -> e1'
---------------------
 e1 + e2 -> e1' + e2
...

--------------------------------
 if true then e2 else e3 -> e2

---------------------------------
 if false then e2 else e3 -> e3

                    e1 -> e1'
-------------------------------------------------
 if e1 then e2 else e3 -> if e1' then e2 else e3

----------------------------------
 (fun x : t => e1) e2 -> e1[e2/x]

    e1 -> e1'
-----------------
 e1 e2 -> e1' e2
```
