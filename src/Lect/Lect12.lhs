% CS 340: Programming Paradigms and Patterns
% Lect 12 - Folds
% Michael Lee

> module Lect.Lect12 where

Folds
=====

Primitive Recursion
-------------------

Let's start by implementing the following functions and looking for a pattern:

> sum' :: (Num a) => [a] -> a
> sum' [] = 0
> sum' (x:xs) = x + sum' xs
>
> product' :: (Num a) => [a] -> a
> product' [] = 1
> product' (x:xs) = x * product' xs
>
> and' :: [Bool] -> Bool
> and' [] = True
> and' (x:xs) = x && or' xs
>
> or' :: [Bool] -> Bool
> or' [] = False
> or' (x:xs) = x || or' xs
> 
> stringify :: (Show a) => [a] -> String
> stringify [] = ""
> stringify (x:xs) = show x ++ stringify xs

Each of the above recursive functions has type `[a] -> b`, and is built around
two basic pieces:

1. a value (of type `b`) associated with the base case (the empty list)
2. a function that takes a value from the list (of type `a`) and combines it with
   the value returned by the recursive call (of type `b`) to compute the result
   (also of type `b`)

I.e., to express a recursive list function of type `[a] -> b`, we need:

- a combining function with type `a -> b -> b`
- a base case value of type `b`

Let's design a HOF that encapsulates this notion of primitive list recursion:

> recur :: (a -> b -> b) -> b -> [a] -> b
> recur f z [] = z
> recur f z (x:xs) = f x $ recur f z xs

We call this function "fold" --- specifically, "fold right".


Fold Right
----------

< foldr :: (a -> b -> b) -> b -> [a] -> b

Note: foldr is actually defined for the "Foldable" type class --- the list is an
instance of Foldable. We'll see how this works later!

Intuitively, foldr applies the combiner function f to the last (i.e., rightmost)
value in the list and the base value *first*, then works outwards, applying f
to each list value and the result from the previous application of f, in turn.

E.g., foldr f z [1..5] = (1 `f` (2 `f` (3 `f` (4 `f` (5 `f` z)))))

                       = (f 1 (f 2 (f 3 (f 4 (f 5 z)))))

Let's define the above recursive functions in terms of foldr:

> sum'' :: (Num a) => [a] -> a
> sum'' = foldr (+) 0
>
> product'' :: (Num a) => [a] -> a
> product'' = foldr (*) 1
>
> or'' :: [Bool] -> Bool
> or'' = foldr (||) False
> 
> and'' :: [Bool] -> Bool
> and'' = foldr (&&) True
> 
> stringify' :: (Show a) => [a] -> String
> stringify' = foldr ((++) . show) ""

Try a few others:

> (+++) :: [a] -> [a] -> [a]
> l1 +++ l2 = foldr (:) l2 l1
>
> length' :: [a] -> Int
> length' = foldr (\_ r -> succ r) 0

And higher order functions:

> map' :: (a -> b) -> [a] -> [b]
> map' f = foldr ((:) . f) []
>
> filter' :: (a -> Bool) -> [a] -> [a]
> filter' f = foldr iter []
>   where iter x rst | f x = x : rst
>                    | otherwise = rst

How about reverse?

> reverse' :: [a] -> [a]
> reverse' [] = []
> reverse' (x:xs) = reverse' xs ++ [x]

Which translates into:

> reverse'' :: [a] -> [a]
> reverse'' = foldr (flip (++) . (:[])) []

But this is inefficient! (Why?)

A better implementation of reverse is:

> reverse''' :: [a] -> [a]
> reverse''' xs = recur [] xs
>   where recur ys [] = ys
>         recur ys (x:xs) = recur (x:ys) xs

Note how the result (`ys`) computed by `recur` is built up (aka "accumulated")
from left to right over the input list. It is difficult (but not impossible!)
to implement `reverse'''` in terms of foldr.
  

Fold Left
---------

In a left fold, we want to start by applying the combiner function to the base
value and the first value in the list, then proceed onto succeeding values, so:

foldl f z [1..5] = (((((z `f` 1) `f` 2) `f` 3) `f` 4) `f` 5)

                 = (f (f (f (f (f z 1) 2) 3) 4) 5)

Determine the type of the left fold function and implement it:

> foldl' :: (b -> a -> b) -> b -> [a] -> b
> foldl' _ z [] = z
> foldl' f z (x:xs) = foldl' f (f z x) xs

Define reverse using foldl:

> reverse'''' :: [a] -> [a]
> reverse'''' = foldl (flip (:)) []


On Infinite Lists
-----------------

Which folds (if any) work with infinite lists?

Try:

< take 10 $ foldr (:) [] [1..]
< foldr (||) False $ map even [1..]
< foldl (||) False $ map even [1..]

Why?

Intuitively:
- foldr's combining function is applied to each element in turn (and to the
  recursive call) --- this lets the combining function "short circuit" early
- foldl builds up an accumulated value; the result is not known until all
  recursions are evaluated
- foldl must recurse through the entire list to build up all the function
  applications before evaluating the *outermost* one


Which Fold?
-----------

- foldl is *left associative*
- foldr is *right associative*
- foldr can work with infinite lists!

consider:

> foldl1' :: (a -> a -> a) -> [a] -> a
> foldl1' f (x:xs) = foldl f x xs

> foldr1' :: (a -> a -> a) -> [a] -> a
> foldr1' f [x] = x
> foldr1' f (x:xs) = f x $ foldr1' f xs

Following are true:

< foldl1 (+) [1..10] == foldr1 (+) [1..10]
< foldl1 (*) [1..10] == foldr1 (*) [1..10]

But following are not:

< foldl1 (-) [10, 2, 1, 3] == foldr1 (-) [10, 2, 1, 3]
< foldl1 (^) [2, 3, 4] == foldr1 (^) [2, 3, 4]

`-` is left associative, and so should be evaluated using the left-fold.

`^` is right associative, and should be evaluated using the right-fold.
