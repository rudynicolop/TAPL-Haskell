# The Untyped Lambda Calculus

## Syntax

Let `e,u` range over expressions, and `x` range over variables.

```
e ::= x | \x.e | e1 e2 | (e)
```

## Semantics

The implementation evaluates expressions under a normal-order small-step semantics.

```
    e -> e'
---------------
 \x.e -> \x.e'

------------------------
 (\x.e1) e2 -> e1[e2/x]

    e2 -> e2'
-----------------
  x e2 -> x e2'

    e1 -> e1'
-----------------
 e1 e2 -> e1' e2
```
At long last a formalized semantics for normal-order evaluation, to supplement vague—"left-most outer-most full beta-reduction" explanations.

## Supported Macros

The parser supports the following macros so one need not write everything out in pure lambda calculus.

Let `n,m` range over natural numbers and `a,b` range over booleans.

```
id = \x.x
0 = \f.\z.z
1 = \f.\z. f z
2 = \f.\z f (f z)
n = \f.\z. f (f (f ...(f n)))
succ n = (\n.\f.\z. f (n f z)) n
add n m = (\n.\m. n succ m) n m
mult n m = (\n.\m. n (add m) 0) n m
pred n = (\n.\f.\z. n (\g.\h. h (g f)) (\u.z) id) n
sub n m = (\n.\m. m pred n) n m
true = \x.\y.x
false = \x.\y.y
cond b e u = (\b.\e.\u. b e u) b e u
and a b = (\a.\b. a b false) a b
or a b = (\a.\b. a true b) a b
iszero n = (\n. n (\x. false) true) n
eq n m = (\n.\m. and (iszero (sub n m) (iszero (sub m n)))) n m
Y f = (\f. (\x. f (x x)) (\x. f (x x))) f
```

## Example Programs

### Arithmetic

The interpreter performs arithmetic!
```
add 3 4
mul 2 3
and true false
```

### Fibonacci

Try computing Fibonacci numbers!

***To those computing the Fibonacci number for 5,6 or above, beware!***

```
(Y \f. \x. cond (or (iszero x) (eq 1 x)) 1 (add (f (pred x)) (f (sub x 2)))) 3
```

### Factorial

Try out the factorial function!

***I caution thee against computing the factorial for numbers greater than 3...***
```
(Y \ f. \n. cond (iszero 0) 1 (mul n (f (pred n)))) 3
```
