# The Simply Typed Lambda Calculus

## Syntax

Let `e` range over expressions, `x` range over variables, and `t` range over types.

```
t ::= Nat | Bool | t1 -> t2 | (t)
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
--------------{T-Nat}
 G |- n : Nat

---------------{T-Bool}
 G |- b : Bool

  G x = t
------------{T-Var}
 G |- x : t

 G |- e1 : Nat   G |- e2 : Nat
-------------------------------{T-Add}
      G |- e1 + e2 : Nat
...

 G |- e1 : Bool   G |- e2 : t   G |- e3 : t  
---------------------------------------------{T-Cond}
       G |- if e1 then e2 else e3 : t

       G[x:t] |- e : t'
-------------------------------{T-Fun}
 G |- fun x : t => e : t -> t'

 G |- e1 : t -> t'   G |- e2 : t
---------------------------------{T-App}
        G |- e1 e2 : t'
```

## Dynamic Semantics

The interpreter performs a lazy small-step evaluation.

```
 n1 + n2 = n3
---------------{E-Add-1}
 n1 + n2 -> n3

     e2 -> e2'
-------------------{E-Add-2}
 n + e2 -> n + e2'

      e1 -> e1'
---------------------{E-Add-3}
 e1 + e2 -> e1' + e2
...

--------------------------------{E-Cond-1}
 if true then e2 else e3 -> e2

---------------------------------{E-Cond-2}
 if false then e2 else e3 -> e3

                    e1 -> e1'
-------------------------------------------------{E-Cond-3}
 if e1 then e2 else e3 -> if e1' then e2 else e3

----------------------------------{E-Redux}
 (fun x : t => e1) e2 -> e1[e2/x]

    e1 -> e1'
-----------------{E-App}
 e1 e2 -> e1' e2
```
