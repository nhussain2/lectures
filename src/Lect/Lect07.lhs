% CS 340: Programming Paradigms and Patterns
% Lect 07 - Higher order functions
% Michael Lee

> module Lect.Lect07 where

Higher order functions
======================

Agenda:
  - Lambda expressions
  - Operator sections
  - HOFs
  - Composition & Application
  - Mapping as iteration
  - Filtering
  - Generalized "zipping"
  - Miscellaneous HOFs


Lambda expressions
------------------


Operator sections
-----------------


HOFs
----  

A higher-order function is a function that takes one or more functions as
parameters or returns a function. ("Regular" functions are called first-order
functions).

HOFs enable us to create higher-level abstractions, and are a fundamental
tool in functional programming.

Note: due to currying, *all functions of 2 or more arguments* are HOFs!


Composition & Application
-------------------------

< (.) :: (b -> c) -> (a -> b) -> a -> c
< ($) :: (a -> b) -> a -> b

(.) succinctly expresses the layering of two or more functions:

< (sqrt . sin) 1
< (take 10 . repeat) 5

( .) often encourages "point-free" (i.e., argument-less) function definitions:

> even' :: Integral a => a -> Bool
> even' = (== 0) . (`rem` 2)
>
> label :: Show a => a -> String
> label = ("Val = " ++) . show
>
> k2c k = k - 273
> c2f c = c * 9 / 5 + 32
> f2h f
>   | f < 0 = "too cold"
>   | f > 100 = "too hot"
>   | otherwise = "survivable"
>
> k2h = f2h . c2f . k2c

($) is typically used to minimize parentheses:

< show (abs (2 - 5))
< show $ abs $ 2 - 5
<
< take 5 (drop 10 (zip [1..] (repeat 'a')))
< take 5 $ drop 10 $ zip [1..] $ repeat 'a'

Define (.) and ($) ourselves:

> comp :: (b -> c) -> (a -> b) -> a -> c
> f `comp` g = \x -> f (g x)
>
> app :: (a -> b) -> a -> b
> infixr 0 `app` -- force low precedence right-associativity
> f `app` x = f x


Mapping as "Iteration"
----------------------

< map :: (a -> b) -> [a] -> [b]

`map` applies a function to each item of a list, returning the new list.

Try out:

< map even [1..10]
< map reverse $ words "madam I refer to adam"
< map (^2) [1..10]
< map (\x->(x,x^2)) [1..10]
< map (++) $ words "on over under across" -- what does this do?
< map (\f -> f " the table") $ map (++) (words "on over under across")
< map (map (*2)) [[1..5], [6..10], [11..15]]

Define map ourselves:

> map' :: (a -> b) -> [a] -> [b]
> map' _ [] = []
> map' f (x:xs) = f x : map' f xs


Filtering
---------

< filter :: (a -> Bool) -> [a] -> [a]

`filter` is another typical HOF. `filter` only keeps values in a list that pass
a given predicate (a function that returns True/False).

Try out:

< filter even [1..10]
< filter (\(a,b,c) -> a^2+b^2==c^2) [(a,b,c) | a<-[1..10], b<-[a..10], c<-[b..10]]
< filter (\s -> reverse s == s) $ words "madam I refer to adam"
< map (\w -> (w,length w)) $ filter (\s -> reverse s == s) $ words "madam I refer to adam"
  
Define filter ourselves:

> filter' :: (a -> Bool) -> [a] -> [a]
> filter' _ [] = []
> filter' p (x:xs) | p x = x : filter' p xs
>                  | otherwise = filter' p xs


Generalized "Zipping"
_____________________

< zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
< zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
< zipWith4 ...

`zipWith` abstracts the zipping function (which, in zip, is just `(,)`)

Try out:

< zipWith (,) [1..5] [6..10]
< zipWith (+) [1..5] [10,9..6]
< zipWith (\x y -> x ++ ":" ++ show y) ["a", "b", "c"] [1..3]
< zipWith3 (,,) [1..5] [10,20..50] [100,200..500]

Yet another fibonacci definition!

> fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

Define zipWith ourselves:

> zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWith' _ [] _ = []
> zipWith' _ _ [] = []
> zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


Misc. HOFs
----------

< iterate :: (a -> a) -> a -> [a]
< until :: (a -> Bool) -> (a -> a) -> a -> a
< takeWhile :: (a -> Bool) -> [a] -> [a]

Try out:

< iterate (*2) 1
< iterate (++".") ""

Using `until`, implement Newton's method for finding square roots:

1. Start with some guess g as the root of x
2. Check if g^2 is close enough to x; if so, we're done
3. Compute an improved guess by averaging g and x/g and repeat 2

> newtonsSqrt :: (Floating a, Ord a) => a -> a
> newtonsSqrt x = until check improve 1
>   where check g = abs (g^2 - x) < 0.00001
>         improve g = (g + x/g) / 2

Try out:

< takeWhile (\g -> abs (g^2-100) >= 0.000001) $ iterate (\g -> (g+100/g)/2) 1

More list HOFs (import Data.List to try):

< sortOn :: Ord b => (a -> b) -> [a] -> [a]
< find :: (a -> Bool) -> [a] -> Maybe a
< partition :: (a -> Bool) -> [a] -> ([a], [a])

Try out:

< sortOn length $ words "what is the correct order?"
< find (\s -> reverse s == s) $ words "where is the civic center?"
< partition even [1..10]
