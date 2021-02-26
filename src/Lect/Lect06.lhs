% CS 340: Programming Paradigms and Patterns
% Lect 06 - Recursion
% Michael Lee

> module Lect.Lect06 where

Recursion
=========

Agenda:
  - Designing recursive functions
  - Structural vs. Generative recursion
  - Accumulators and Tail recursion


Designing recursive functions
-----------------------------

Steps:
  1. determine the function type 
  2. identify which inputs can be decomposed into subproblems
  3. define the function for input patterns that can be handled non-recursively
  -- base cases, use pattern matching (examples of how function should behave)
  -- list out bunch of examples on input/output (more examples, the better)
  4. define the function for input patterns that require recursion
  -- recursive cases
  5. ensure that the values are "shrunk" in recursive calls
  -- make sure recursion completes (possibly option)
  6. generalize and simplify

In steps 3 and 4, we might discover that we need additional functions, which 
themselves require stepping through the design process!

---

E.g., summing integer values from 0 up to N:

> sumTo :: Integral a => a -> a
> sumTo 0 = 0 -- base case
> sumTo 1 = 1 -- base case
> sumTo x = x + sumTo(x-1) -- recursive, sumTo(x-1) is the sub problem

--- 

E.g., compute all permutations of values in a list

"abc" => [abc, cba, bca, bac, acb, cab]

> permutations :: [a] -> [[a]] -- takes a list of things, returns list of list of things, each sublist is a permutation
> permutations [] = [[]] -- empty list base case, one permutation, empty list
> -- permutations [x] = [[x]] -- single element
> -- permutations [x,y] = [[x,y], [y,x]] -- 2 elements

> -- permutations [x,y,z] = [[x,y,z], [y,x,z], [y,z,x],
                          [x,z,y], [z,x,y], [z,y,x]]
-- keep y,z in order and interleave x into it, then flip to z,y and interleave x into it again

-- break list up into pieces,     xs is smaller version of problem
> permutation (x:xs) = concat [interleave x p | p <- permutations xs]
-- use concat to concat the list of lists 

-- need a function, interleave
> interleave :: a -> [a] -> [[a]]
-- takes value, takes list of values and returns list of lists
> interleave x[] = [[x]]
-- interleave x[y] = [[x,y],[y,x]]
-- interleave x[y,z] = [[x,y,z],[y,x,z], [y,z,x]]
> interleave x l@(y:ys) = (x:l) : [ y:ll | ll <- interleave x ys]
-- can use list comprehension
-- y in front of the x interleaved of ys

Structural vs. Generative recursion
-----------------------------------

The above recipe may apply when a problem can be tidily decomposed into
subproblems according to the implicit structure of the values involved. E.g.,
solving a problem represented as a list by processing its head and recursing on
its tail.

But not all recursive functions follow this pattern! Sometimes solving a problem
recursively requires that the input values be transformed into new values which
aren't clearly substructures of nor "smaller" than the originals. 

This is sometimes called "generative" recursion. Their design is the domain of
algorithms.

---

E.g., Newton's method for finding the square root of N starts with a guess g
      (say, N/2), then tests to see if it is good enough (i.e., if the square of
      the g^2 == N); if not, we improve the guess by average g with n/g and try
      again. The intuition is that if g is too small, n/g will be increase the 
      guess, and if g is too big, n/g will decrease.

improve :: (Floating a, Ord a) => a -> a
iter :: (Floating a, Ord a) => a -> a
goodEnough :: (Floating a, Ord a) => a -> Bool

> newtonsSqrt :: (Floating a, Ord a) => a -> a
> newtonsSqrt n = iter (n/2) --let g = n/2
              --in if goodEnough g then g else iter g
        where goodEnough g  = g^2 =~= n
              improve g = (g + n/g) / 2
              iter g | goodEnough g = g -- check to see if good enough
                     | otherwise = iter (improve g) -- recurse, improve it

      
-- HELPER FUNCTIONS WE CAN USE
-- good enough function, checks is good enough and returns T or F
-- goodEnough ::  (Floating a, Ord a) => a -> Bool

-- improves and returns new value
--improve :: (Floating a, Ord a) => a -> a
-- tries again
-- iter :: (Floating a, Ord a) => a -> a

> infix 4 =~= -- approx equals (might come in handy)
> (=~=) :: (Floating a, Ord a) =>  a -> a -> Bool
> x =~= y = abs (x - y) < 0.000001

---

E.g., sort a list of values by splitting it in two, sorting each half, then 
merging the sorted results:

> mergesort :: undefined
> mergesort = undefined


Accumulators and Tail recursion
-------------------------------

Some recursive functions are more naturally written and/or efficient when
implemented with an *accumulator*. 

E.g., consider our original implementation of `reverse`:

> reverse' :: [a] -> [a]
> reverse' [] = []
> reverse' (x:xs) = reverse' xs ++ [x]

This is inefficient, because the concatenation operator (++) needs to "search
for the end" of its first argument list (which is also the result of the
recursive call) in order to do its job. It would be more efficient to use the
`:` operator to incrementally build up a partially reversed list over the course
of the recursion. 

> reverse'' :: [a] -> [a] -> [a]
> reverse'' = undefined

The second argument of `reverse''` needs to be "primed" with an empty list, and
then gradually accumulates the solution, which we obtain at the end of the 
recursion. 

So that the caller doesn't need to provide the priming value, accumulators are
typically hidden inside where clauses:

> reverse''' :: [a] -> [a]
> reverse''' xs = rev xs []
>   where rev = undefined

Try doing ":set +s" in ghci, then comparing outputs for the following:

  - take 5 $ reverse'   [1..1000000]
  - take 5 $ reverse''' [1..1000000]

---

We say that a function like `reverse'''`, where the solution to the problem is
obtained at the end of the recursion instead of being computed on the way "up"
out of a recursion, is *tail recursive*.

Sometimes tail recursion is good in Haskell, as it allows the function to be
more efficient (as above). Sometimes, however, it works against us. 

Consider a function that takes a value x and partitions an input list into two
output lists: one containing values < x, and the other containing values >= x.
Here we have two implementations --- one tail recursive and one not:

> tailPartition :: Ord a => a -> [a] -> ([a],[a])
> tailPartition n xs = part xs ([],[])
>   where part [] r = r
>         part (y:ys) (lts,gts) | y < n     = part ys (y:lts, gts)
>                               | otherwise = part ys (lts, y:gts)
>
>
> nontailPartition :: Ord a => a -> [a] -> ([a],[a])
> nontailPartition n [] = ([],[])
> nontailPartition n (x:xs) | x < n     = (x:lts, gts)
>                           | otherwise = (lts, x:gts)
>   where (lts, gts) = nontailPartition n xs

What happens when we call the two variations on an infinite list, but we only
need to take a fixed number of values from a given partition?

  - E.g., "take 5 $ snd $ XXXPartition 100 [1..]"