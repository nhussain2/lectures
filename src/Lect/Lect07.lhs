% CS 340: Programming Paradigms and Patterns
% Lect 07 - Higher order functions
% Michael Lee

> module Lect.Lect07 where
> import Data.Char
> import Data.List

Higher order functions
======================

Agenda:
  - HOF overview
  - Functions as values
    - Partial application
    - Operator sections
    - Lambda expressions
  - Function application
  - Function composition
  - Mapping
  - Filtering
  - Generalized "zipping"
  - Miscellaneous HOFs


HOF overview
------------

A higher-order function is a function that takes a function as a parameter or
returns a function. ("Regular" functions are called first-order functions).

HOFs enable us to create high-level abstractions, and are a fundamental tool in
functional programming.


Functions as values
-------------------

To use or implement HOFs it is necessary to obtain function values; there are
numerous ways of doing this.


-- Named functions

When we define functions at the top-level or in where/let clauses, their names
are just symbols bound to function values:

> foo :: Int -> Int
> foo = undefined
> 
> bar :: Int -> Int
> bar = foo


-- Partial application

Due to currying, *all* functions of two or more arguments are HOFs. Partially
applying a function returns a function of fewer arguments.

Consider:

> strCat :: String -> String -> String
> strCat s t = s ++ t
>
> sayHiTo :: String -> String
> sayHiTo = strCat "hi, "


-- Operator sections

We can also partially apply operators by placing them in parentheses and
leaving out one of their arguments, which give us partial functions "waiting
for" the missing argument.

E.g.,

> twoPlus = (2+)
> divByThree = (/3)
> divThreeBy = (3/)
> sayHiTo' = ("hi, " ++)


-- Lambda expressions

Lambda expressions allow us to define anonymous functions.

Syntax: \ var1 var2 ... -> expression

  Note: the expression after `->` extends as far to the right as possible,
        so parenthesization is often needed!

We can use it as an alternative way to define top-level functions:

> sumOfSquares :: Num a => a -> a -> a
> sumOfSquares = \x y -> x^2 + y^2


We often use a lambda when we want to make it clear that a function is designed
to return a function:

> greeter :: String -> (String -> String) -- note: parens are superfluous
> greeter g = \name -> g ++ ", " ++ name

> sayHiTo'' = greeter "hi"


We also use lambdas to create simple functions to be passed to HOFs:

E.g., the library function `dropWhile` is the HOF version of drop:

    dropWhile :: (a -> Bool) -> [a] -> [a]

So we can do:

    dropWhile (\x -> isDigit x || isPunctuation x) "01. Lorem ipsum"


Function application
--------------------

Function application is defined via the `$` operator, thusly:

    ($) :: (a -> b) -> a -> b
    infixr 0 $
    f $ x = f x


The low(est) precedence and right associativity of `$` make it so we can often
use it to avoid the excessive parenthesization that would be necessary with
default function application (via space).

E.g., how can we rewrite the following using `$`?

    sayHiTo ("Michael" ++ "Lee")
    show (abs (2 - 5))
    take 5 (drop 10 (zip [1..] (repeat 'a')))


Function composition
--------------------

Function composition is an operation on two functions that applies the first to
the result of applying the second. It is defined thusly:

    (.) :: (b -> c) -> (a -> b) -> a -> c
    infixr 9 .
    f . g = \x -> f (g x)


`.` lets us succinctly combine functions:

    (sqrt . sin) 1
    (take 10 . repeat) 5


`.` also facilitates "point-free" (i.e., argument-less) function definitions.

E.g., re-implement `even'`, `k2h`, and `strip` with `.`

> even' :: Integral a => a -> Bool
> even' x = (x `rem` 2) == 0
>
>
> k2c k = k - 273
> c2f c = c * 9 / 5 + 32
> f2h f
>   | f < 0 = "too cold"
>   | f > 100 = "too hot"
>   | otherwise = "survivable"
>
>
> k2h  c = f2h (c2f (k2c c))
>
>
> strip :: String -> String
> strip s = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))


`.` also often does away with the need to create lambdas. 

E.g., how can we do away with the lambda in the following?

    dropWhile (\w -> length w > 3) $ words "hello how are you today?"


Mapping
-------

`map` applies a function to each item of a list, returning the new list.

    map :: (a -> b) -> [a] -> [b]


Interpret:

    map even [1..10]
    map reverse $ words "madam I refer to adam"
    map (^2) [1..10]
    map (\x->(x,x^2)) [1..10]
    map (++) $ words "on over under across" -- what does this do?
    map (\f -> f " the table") $ map (++) (words "on over under across")
    map (map (*2)) [[1..5], [6..10], [11..15]]


Implement `map` ourselves:

> map' :: (a -> b) -> [a] -> [b]
> map' = undefined


`map` allows us to avoid performing explicit recursion in most situations where
an input list is to be "transformed" into an output list of the same size. (When
might it not be usable?)


Filtering
---------

`filter` only keeps values in a list that pass a provided predicate:

    filter :: (a -> Bool) -> [a] -> [a]


Interpret:

  filter even [1..10]

  filter (\(a,b,c) -> a^2+b^2==c^2) $
         [(a,b,c) | a <- [1..10], b <- [a..10], c <- [b..10]]

  filter (\s -> reverse s == s) $ 
         words "madam I refer to adam"

  map (\w -> (w,length w)) $ 
      filter (\s -> reverse s == s) $ 
      words "madam I refer to adam"
  

Implement `filter` ourselves:

> filter' :: (a -> Bool) -> [a] -> [a]
> filter' = undefined


`filter` replaces another category of explicitly recursive functions --- those
that take an input list and extract a subset of the elements (possibly for
further processing). 

E.g., consider this property from MP1:

    prop_luhnOneSol :: Property
    prop_luhnOneSol = forAll genLuhnCandidates $ \cs ->
      length (filter p3_luhn cs) == 1
