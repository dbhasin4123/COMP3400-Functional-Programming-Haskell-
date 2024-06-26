module Polynomial (expand, simplify, Polynomial(..)) where

{--

*DO NOT* import any modules.
*You are allowed to use anything available in Prelude and any syntax features* 

You may remove any of the comments if you like.

Below is a data type for representing univariate polynomials from NN[x].  That
is, polynomials in x with NATURAL (non-negative integer) coefficients.

Arithmetic on NN[x] is easier than it is on ZZ[x] (polynomials with integer
coefficients) becuase you do not have to worry about terms cancelling.

type Deg = Integer   -- precondition: always nonnegative.
type Coeff = Integer -- precondition: always positive.
data Polynomial = Mono Coeff Deg | Add Polynomial Polynomial | Mul Polynomial Polynomial

For instance:
1/  3x^2 + 2x + 1
is encoded by
  Add (Mono 3 2) $ Add (Mono 2 1) $ (Mono 1 0)

2/  (2x + 1) * (x^2 + 2)
is encoded by
  Mul (Add (Mono 2 1) (Mono 1 0)) (Add (Mono 1 2) (Mono 2 0))

============
PRECONDITION
============

1.  Assume monomials always have degree >= 0 and coefficients >0.

====
TASK
====

Write TWO functions
  expand :: Polynomial -> Polynomial
  simplify :: Polynomial -> Polynomial

============================
expand :: Polynomial -> Polynomial
============================

This function takes a polynomial and returns an equivalent polynomimal where
multiplications have been removed.

For example
1/  f = (2x + 1) * (x^2 + 2) = 2x^3 + 4x + x^2 + 2
2/  g = (x + 1) * (x + 1) = x^2 + x + x + 1
NOTE, *do not* combine like terms (i.e. simplify) --- just remove Mul without
changing the polynominal.

=======
EXAMPLE
=======
NOTE your solution does not have to look identical to the following examples.
We will be conducting PROPERTY TESTING of you code.  That is, we will confirm
your output is EQUAL to the input and DOES NOT CONTAIN the Mul constructor.

> f = Mul (Add (Mono 2 1) (Mono 1 0)) (Add (Mono 1 2) (Mono 2 0))
> expand f
Add (Add (Mono 2 3) (Mono 4 1)) (Add (Mono 1 2) (Mono 2 0))

> g = Mul (Add (Mono 1 1) (Mono 1 0)) (Add (Mono 1 1) (Mono 1 0))
> expand g
Add (Add (Mono 1 2) (Mono 1 1)) (Add (Mono 1 1) (Mono 1 0))

==============================
simplify :: Polynomial -> Polynomial
==============================

Every polynomial can be written in SIMPLIFIED FORM like
   a{n}*x^{n} + a{n-1}*x^{n-1} + ... + a{1}x + a{0}
In particular, the monomials are given in DESCENDING degree order.

For example,
1/  f = (2x + 1) * (x^2 + 2)
simplifies to
    2x^3 + x^2 + 4x + 2

2/  g = x^2 + x + x + 1
simplifies to
    x^2 + 2x + 1

Notice like terms 'x' and 'x' are now combined to '2x'.

=======
EXAMPLE
=======
NOTE simplify returns a CANONICAL FORM and therefore your answers MUST BE
IDENTICAL to the output here.

> f = Mul (Add (Mono 2 1) (Mono 1 0)) (Add (Mono 1 2) (Mono 2 0))
> simplify f
Add (Mono 2 3) (Add (Mono 1 2) (Add (Mono 4 1) (Mono 2 0)))

> g = Mul (Add (Mono 1 1) (Mono 1 0)) (Add (Mono 1 1) (Mono 1 0))
> simplify g
Add (Mono 1 2) (Add (Mono 2 1) (Mono 1 0))

--}

type Deg = Integer   -- precondition: always nonnegative.
type Coeff = Integer -- precondition: always positive.

data Polynomial = Mono Coeff Deg | Add Polynomial Polynomial | Mul Polynomial Polynomial
    deriving Show

-- eliminate multiplication
expand :: Polynomial -> Polynomial
expand = undefined

-- simplified polynom is returned in descending degree
simplify :: Polynomial -> Polynomial
simplify = undefined
