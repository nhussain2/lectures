> module Lect.Temp where

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

 fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

Define zipWith ourselves:

 zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
 zipWith' _ [] _ = []
 zipWith' _ _ [] = []
 zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


Misc. HOFs
----------

< flip :: (a -> b -> c) -> b -> a -> c

takeWhile / dropWhile

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

 newtonsSqrt :: (Floating a, Ord a) => a -> a
 newtonsSqrt x = until check improve 1
   where check g = abs (g^2 - x) < 0.00001
         improve g = (g + x/g) / 2

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
