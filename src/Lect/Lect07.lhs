% CS 340: Programming Paradigms and Patterns
% Lect 07 - Recursion
% Michael Lee

> module Lect.Lect07 where

Recursion
=========

Designing recursive functions
-----------------------------

Step 1: determine the type
Step 2: list all the patterns
Step 3: define the trivial cases
Step 4: define the hard cases
Step 5: generalize and simplify


Basics
------

> fib :: Integer -> Integer
> fib 0 = 0
> fib 1 = 1
> fib n = fib (n-1) + fib (n-2)
>
> factorial :: Integer -> Integer
> factorial 0 = 1
> factorial n = n * factorial (n-1)
>
> -- define integral `pow` (exponentiation) in terms of multiplication
> pow :: Integral a => a -> a -> a
> pow _ 0 = 1
> pow n e = n * pow n (e-1)
> 
> -- define integral `add` only in terms of succ and pred
> add :: Integral a => a -> a -> a
> add m 0 = m
> add m n | n > 0 = add (succ m) (pred n)
>         | otherwise = add (pred m) (succ n)
>
> -- define integral `mod` using subtraction
> mod' :: Integral a => a -> a -> a
> mod' m n | m < n = m
>         | otherwise = mod' (m-n) n


List Manipulation
-----------------

> last' :: [a] -> a
> last' [] = error "empty list"
> last' (x:[]) = x
> last' (x:xs) = last' xs
>
> (!!!) :: [a] -> Integer -> a
> [] !!! _ = error "index too large"
> (x:_) !!! 0 = x
> (_:xs) !!! n = xs !!! pred n
>
> length' :: [a] -> Int
> length' [] = 0
> length' (_:xs) = 1 + length' xs
> 
> -- elem' :: return True if a given element is found in a list, False otherwise
> elem' :: Eq a => a -> [a] -> Bool
> _ `elem'` [] = False
> x `elem'` (y:ys) = x == y || x `elem'` ys
>
> -- and' :: determine if all values in a list are True
> and' :: [Bool] -> Bool
> and' [] = True
> and' (x:xs) = x && and' xs
> 
> -- sum' :: compute sum of a list of numbers
> sum' :: Num a => [a] -> a
> sum' [] = 0
> sum' (x:xs) = x + sum' xs


List Construction
-----------------

> (+++) :: [a] -> [a] -> [a] 
> [] +++ ys = ys
> xs +++ [] = xs
> (x:xs) +++ ys = x : (xs +++ ys)
>
> take' :: Int -> [a] -> [a]
> take' _ [] = []
> take' 0 _ = []
> take' n (x:xs) = x : (take' (n-1) xs)
> 
> drop' :: Int -> [a] -> [a]
> drop' _ [] = []
> drop' 0 xs = xs
> drop' n (_:xs) = drop (n-1) xs
> 
> -- replicate' :: create a list of N copies of some value
> replicate' :: Int -> a -> [a]
> replicate' 0 _ = []
> replicate' n x = x : replicate' (n-1) x
>
> -- repeat' :: create an infinite list of some value
> repeat' :: a -> [a]
> repeat' x = x : repeat' x
>
> -- concat' :: concatenate all lists in a list of lists
> concat' :: [[a]] -> [a]
> concat' [] = []
> concat' (x:xs) = x ++ concat' xs
>
> -- merge :: merge together two sorted lists to give a single sorted list
> merge :: Ord a => [a] -> [a] -> [a]
> merge xs [] = xs
> merge [] ys = ys
> merge l1@(x:xs) l2@(y:ys) | x < y = x : merge xs l2
>                           | otherwise = y : merge l1 ys
>
> -- mergeSort :: sort a list by recursively merging sorted halves of a list
> mergeSort :: Ord a => [a] -> [a]
> mergeSort [] = []
> mergeSort [x] = [x]
> mergeSort l = merge (mergeSort left) (mergeSort right)
>   where left = take half l
>         right = drop half l
>         half = length l `div` 2
> 
> -- zip' :: create a list of tuples drawn from elements of two lists
> zip' :: [a] -> [b] -> [(a,b)]
> zip' _ [] = []
> zip' [] _ = []
> zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
>
> fibonacci :: [Integer]
> fibonacci = 0 : 1 : next fibonacci
>   where next (x0:x1:xs) = x0+x1 : next (x1:xs)


