module Parser (Parser (..), Polynomial(..), polynomial, parse, expand, simplify) where

import           Control.Applicative

{--

Do not import anything besides Control.Applicative.
You are allowed to use anything available in Prelude and Control.Applicative, as well as any syntax features.

====
TASK
====

Write an instance of show for the Polynomial data type that produces *exactly* the
behaviour in the example.

In particular, only print brackets when necessary.

Note that we're allowing zero coefficients on monos to simplify the parser.

=======
EXAMPLE
=======
> Mono 1 1
x

> Mono 1 0
1

> (m,n,j,k) = (Mono 1 2, Mono 3 4, Mono 5 6, Mono 7 8)
(x^2,3x^4,5x^6,7x^8)

> Add (m) (Add (n) (j))
x^2 + 3x^4 + 5x^6

> Add (Add (m) (n)) (j)
x^2 + 3x^4 + 5x^6

> Mul (Add (m) (n)) (Add (j) (k))
(x^2 + 3x^4)(5x^6 + 7x^8)

> Mul m n
(x^2)(3x^4)

> Mul (Mul m n) j
(x^2)(3x^4)(5x^6)

> Mul m (Mul n j)
(x^2)(3x^4)(5x^6)

====
TASK
====

Write a parser
    polynomial :: Parser Polynomial
for reading the string representation of a polynomial back into the Polynomial
data type.

Use polynomial to define an instance of read for the Polynomial data type.

=======
EXAMPLE
=======
> (parse polynomial) ")("
Nothing

> (parse polynomial) "2x^3"

> (parse polynomial) "(2x^3)"
Just (Mono 2 3,"")

> (parse polynomial) ("0x^2")  -- It's okay to do this
Just (Mono 0 2,"")

> (parse polynomial) ("3x^24x^3")
Just (Mul (Mono 3 24) (Mono 1 3),"")

> (parse polynomial) "(2x^2+3)(x^3)"
Just (Mul (Add (Mono 2 2) (Mono 3 0)) (Mono 1 3),"")

> (parse polynomial) "(1+x^2)+x^3"
Just (Add (Add (Mono 1 0) (Mono 1 2)) (Mono 1 3),"")

> (parse polynomial) "1+(x^2+x^3)"
Just (Add (Mono 1 0) (Add (Mono 1 2) (Mono 1 3)),"")

> (parse polynomial) "(x)(x^2)+x^3"
Just (Add (Mul (Mono 1 1) (Mono 1 2)) (Mono 1 3),"")

> (parse polynomial) "(x)(x)(x)"
Just (Mul (Mono 1 1) (Mul (Mono 1 1) (Mono 1 1)),"")

> (parse polynomial) "(((x)))"
Just (Mono 1 1,"")

This task is worth 10 POINTS.

The part you need to implement starts on the line 242.

--}

--  start: DO NOT MODIFY --

type Deg = Integer   -- precondition: always nonnegative.
type Coeff = Integer -- precondition: always nonnegative.

data Polynomial = Mono Coeff Deg | Add Polynomial Polynomial | Mul Polynomial Polynomial deriving (Eq)

-- expand and simplify from A2
--
expand :: Polynomial -> Polynomial
expand (Mono c d) = Mono c d
expand (Add f g) = Add (expand f) (expand g)
expand (Mul (Mono c0 d0) (Mono c1 d1)) = Mono (c0*c1) (d0+d1)
expand (Mul (Add f g) h) = Add (expand $ Mul f h) (expand $ Mul g h)  -- right dist
expand (Mul f (Add g h)) = Add (expand $ Mul f g) (expand $ Mul f h)  -- left dist
expand (Mul f g) = expand $ Mul (expand f) (expand g)


-- simplified polynomial is returned in descending degree

simplify :: Polynomial -> Polynomial
simplify (Mono c d) = Mono c d
simplify (Add g h)  = merge' (simplify g) (simplify h)
simplify f          = simplify $ expand f


-- Precondition: input is simplified

merge' :: Polynomial -> Polynomial -> Polynomial
merge' (Mono a b) (Mono c d)
    | b > d      = Add (Mono a b) (Mono c d)
    | d > b      = Add (Mono c d) (Mono a b)
    | otherwise  = Mono (a+c) d
merge' (Mono lcf df) g
    | df > dg   = Add (Mono lcf df) g
    | dg > df   = Add (Mono lcg dg) $ merge' (Mono lcf df) gt
    | otherwise = Add (Mono (lcf+lcg) df) gt
  where
    Add (Mono lcg dg) gt = g
merge' f (Mono c d) = merge' (Mono c d) f
merge' f g
    | df > dg   = Add (Mono lcf df) (merge' ft g)
    | dg > df   = Add (Mono lcg dg) (merge' gt f)
    | otherwise = Add (Mono (lcf+lcg) df) (merge' ft gt)
  where
    Add (Mono lcf df) ft = f
    Add (Mono lcg dg) gt = g


-- Parser type

newtype Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap g pa = do
      a <- pa
      return $ g a

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P (\cs -> Just (a,cs))

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> pa = do
      g <- pg
      g <$> pa

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P $ \cs ->
        case parse p cs of
          Nothing        -> Nothing
          Just (a, str') -> parse (f a) str'

instance Alternative Parser where
    empty :: Parser a
    empty = P $ \str -> Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P $ \cs ->
        case parse p cs of
          Nothing -> parse q cs
          mx      -> mx

-- aux function for removing decorator
parse :: Parser a -> String -> Maybe (a, String)
parse (P p) cs = p cs

-- parase one character
item :: Parser Char
item = P $ foo
  where
    foo (c:cs) = Just $ (c, cs)
    foo _      = Nothing

-- parse a char c when P c.
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

-- parse a digit
digit :: Parser Char
digit = sat (\x -> elem x ['0'..'9'])

-- parse the character x
char :: Char -> Parser Char
char x = sat (== x)

-- parse the string xs
string :: String -> Parser String
string []     = return []
string (x:xs) = (\x xs -> x:xs) <$> (char x) <*> (string xs)

-- parse a natural number
nat :: Parser Integer
nat = read <$> (some digit)

-- throw away space
space :: Parser ()
space = (\x -> ()) <$> (many $ char ' ')

-- ignore surrounding whitespace
token :: Parser a -> Parser a
token pa = do
    space
    a <- pa
    space
    return a

-- parse a symbol, ignoring whitespace
symbol :: String -> Parser String
symbol xs = token $ string xs

-- end DO NOT MODIFY --

-- Your code goes below

polynomial :: Parser Polynomial
polynomial = do
  t <- factors
  do
    symbol "+"
    p <- polynomial
    return (Add t p)
    <|> return t

factors :: Parser Polynomial
factors = do
  f <- factor
  do
    g <- factors
    return (Mul f g)
    <|> return f

factor :: Parser Polynomial
factor = do
  symbol "("
  f <- polynomial
  symbol ")"
  return $ f
  <|> mono

-- read cx^d
mono :: Parser Polynomial
mono = do
  coeff <- nat
  symbol "x"
  symbol "^"
  deg <- nat
  return (Mono coeff deg)
  <|> mono'

-- read 1x^d
mono' :: Parser Polynomial
mono' = do
  symbol "x"
  symbol "^"
  deg <- nat
  return (Mono 1 deg)
  <|> mono''

-- read cx^1
mono'' :: Parser Polynomial
mono'' = do
  coeff <- nat
  symbol "x"
  return (Mono coeff 1)
  <|> mono'''

-- read 1x^1
mono''' :: Parser Polynomial
mono''' = do
  symbol "x"
  return (Mono 1 1)
  <|> constant

-- read cx^0
constant :: Parser Polynomial
constant = do
  coeff <- nat
  return (Mono coeff 0)


{-
Polynomial ::= Factors | Factors “+” Polynomial | Mono
Factors ::= Factor | Factor Factors
Factor ::= “(“ Polynomial” “)”
Mono ::= nat “x^” nat | Mono’
Mono’ ::= “x^” nat | Mono’’
Mono’’ ::= nat “x” | Mono’’’
Mono’’’ ::= “x” | Constant
Constant ::= nat
-}

instance Show Polynomial where
    show (Mono 1 1) = "x"
    show (Mono c 1) = concat $ [show c, "x"]
    show (Mono c 0) = show c
    show (Mono 1 d) = "x^" ++ show d
    show (Mono c d) = concat [show c, "x^", show d]

    show (Add f g) = unwords [show f, "+", show g]

    show (Mul (Mul f g) (Mul f' g')) = concat [show (Mul f g), show (Mul f' g')]
    show (Mul (Mul f g) (Mono c d)) = concat [show (Mul f g), "(", show (Mono c d), ")"]
    show (Mul (Mono c d) (Mul f g)) = concat ["(", show (Mono c d), ")", show (Mul f g)]

    show (Mul (Mul f g) (Add c d)) = concat [show (Mul f g), "(", show (Add c d), ")"]
    show (Mul (Add c d) (Mul f g)) = concat ["(", show (Add c d), ")", show (Mul f g)]

    -- show (Mul (Mul f g) h) = concat [show (Mul f g), show (h)]
    -- show (Mul h (Mul f g)) = concat [show (h), show (Mul f g)]
    show (Mul f g) = concat ["(", show f, ")", "(", show g, ")"]

