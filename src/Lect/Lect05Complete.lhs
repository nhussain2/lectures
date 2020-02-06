% CS 340: Programming Paradigms and Patterns
% Lect 05 - Lists
% Michael Lee

> module Lect.Lect05Complete where
> import Data.Char

Lists
=====

Agenda:
  - The List type
  - Constructing lists
  - Syntactic sugar
  - List comprehensions
  - Common list functions
  - List processing functions
    - Pattern matching
    - Structural recursion


The List type
-------------

Haskell's built-in list type might be defined something like this:

    [a] = [] | a : [a]

Read as: A list containing instances of type 'a' ([a]) is either an empty
         list ([]) or an instance of type 'a' followed by ':' and a list
         containing instances of type 'a' ([a]).

Takeaways: 
  - a list is defined recursively, i.e., in terms of itself
  - the list type is polymorphic
  - lists are homogeneous (all elements of a given list are of the same type)


Constructing lists
------------------

`[]` and `:` are examples of data constructors (aka value constructors); i.e.,
they are functions that return values of the associated type (in this case, the
polymorphic list type).

`[]` is called "empty list" and the `:` operator is called "cons":

    [] :: [a]

    (:) :: a -> [a] -> [a]

We can call `:` in prefix form to build lists:

    (:) 1 []

    (:) 1 ((:) 2 [])

    (:) 1 ((:) 2 ((:) 3 []))

But we usually prefer to use `:` in infix form:

    1 : (2 : (3 : []))

And `:`  is right-associative, so we can just write:

    1 : 2 : 3 : []

E.g., lists of integers:

    []

    100 : []

    20 : -3 : 8 : -15 : []

E.,g., lists of Chars (aka strings):

    []

    'a' : []

    'h' : 'e' : 'l' : 'l' : 'o' : []

---

Functions that construct lists typically:

  - operate recursively

  - use one of the list value constructors (`[]` or `:`) in each recursive call

  - when using `:`, add one element to the front of the list and use recursion
    to construct the rest of the list

  - use `[]` in the base (non-recursive) case, if it exists


> replicate' :: Int -> a -> [a]
> replicate' 0 _ = []
> replicate' n x = x : replicate' (n-1) x
>
> enumFromTo' :: (Ord a, Enum a) => a -> a -> [a]
> enumFromTo' x y | x <= y    = x : enumFromTo' (succ x) y
>                 | otherwise = []
>
> -- and now for some infinite lists
>
> ones :: [Int]
> ones = 1 : ones
> 
> repeat' :: a -> [a]
> repeat' x = x : repeat' x
>
> enumFrom' :: Enum a => a -> [a]
> enumFrom' x = x : enumFrom' (succ x)

Note: to limit the number of values drawn from an infinite list, we can use
      `take` (we'll implement it later)


Syntactic sugar
---------------

Instead of constructing lists with `:`, Haskell gives us syntactic shortcuts:

E.g., for simple itemized lists, [...]

    [1,2,3,4,5,6,7,8,9,10]   ==  1:2:3:4:5:6:7:8:9:10:[]

    [[1, 2, 3], [4, 5, 6]]   ==  (1:2:3:[]) : (4:5:6:[]) : []

    [(2, True), (1, False)]  ==  (2,True) : (1,False) : []


E.g., for lists of characters (string), "...":

    "hello world"       ==  'h':'e':'l':'l':'o':[]

    ["hello", "world"]  ==  ('h':'e':'l':'l':'o':[]) : ('w':'o':'r':'l':'d':[]) : []


E.g., for types that are instances of `Enum`, [I..J] and [I,J..K] and [I..]:

    [1..10]     ==  [1,2,3,4,5,6,7,8,9,10]

    ['a'..'z']  ==  "abcdefghijklmnopqrstuvwxyz"

    [2,4..10]   ==  [2,4,6,8,10]
    
    [10,9..1]   ==  [10,9,8,7,6,5,4,3,2,1]


E.g., for infinite lists of `Enum` types, [I..] and [I,J..]

    [1..]    ==  enumFrom 1

    [3,6..]  ==  enumFromThen 3 6


List comprehensions
-------------------

Syntax:

  [ Expression | Generator, ... , Predicate, ... ]

  - which produces a list of values computed by the Expression

  - where each Generator is of the form "var <- List"

  - and each Predicate is a Boolean expression

  - you can also use `let` to create local vars (without `in`)

E.g.,

> evens = [2*x | x <- [1..]]
>
> evens' = [x | x <- [1..], x `mod` 2 == 0]
> 
> sudokuBoxes = [[[r,c] | r <- rs, c <- cs] | rs <- ["ABC", "DEF", "GHI"],
>                                             cs <- ["123", "456", "789"]]
>
> integerRightTriangles p = [(a,b,c) | a <- [1..p], 
>                                      b <- [a..(p-a)],
>                                      let c = p-(a+b),
>                                      a^2 + b^2 == c^2]
>
> factors :: Integral a => a -> [a]
> factors n = [f | f <- [1..n], n `mod` f == 0]
>
> cartesianProduct :: [a] -> [b] -> [(a,b)]
> cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]
>
> concat' :: [[a]] -> [a]
> concat' ls = [x | l <- ls, x <- l]


Common list functions
---------------------

The "Prelude" module defines many useful list functions (some of which we 
implemented above). They include:

  - Basic operations:

    head :: [a] -> a
    tail :: [a] -> [a]
    null :: [a] -> Bool
    length :: [a] -> Int
    last :: [a] -> a
    init :: [a] -> [a]
    (++) :: [a] -> [a] -> [a]
    (!!) :: [a] -> Int -> a

  - Building lists:

    repeat :: a -> [a]
    replicate :: Int -> a -> [a]
    cycle :: [a] -> [a]

  - Lists -> Lists:

    concat :: [[a]] -> [a]
    reverse :: [a] -> [a]
    zip :: [a] -> [b] -> [(a,b)]

  - Extracting sublists:

    take :: Int -> [a] -> [a]
    drop :: Int -> [a] -> [a]
    break :: (a -> Bool) -> [a] -> ([a], [a])

  - Class specific:

    elem :: Eq a => a -> [a] -> Bool
    maximum :: Ord a => [a] -> a
    minimum :: Ord a => [a] -> a
    sum :: Num a => [a] -> a
    product :: Num a => [a] -> a
    lines :: String -> [String]
    words :: String -> [String]

Note: many of these functions operate on a type class that includes lists and
      other recursive data types (We'll see how this works later.)


List processing functions
-------------------------

-- Pattern matching

`[]` and `:` can be used to pattern match against lists (we'll see that all
value constructors can be used for pattern matching). So the first three basic
operations are trivial to re-implement:

> head' :: [a] -> a
> head' (x:_) = x
>
> tail' :: [a] -> [a]
> tail' (_:xs) = xs
> 
> null' :: [a] -> Bool
> null' [] = True
> null' _  = False


But most other list-processing functions need to potentially extract multiple
values from the list. A common technique for doing this is structural recursion.


-- Structural recursion

Structural recursion loosely describes a pattern for writing functions that
handle recursively defined data types (like the list). When called with a value
of such a type, a structurally recursive function will:

  1. start by determining how the value was constructed

  2. if the value is not a recursive instance of the data type, simply process
     its immediate contents

  3. if the value is a recursive instance of the data type, "deconstruct" it to 
     process its immediate contents, then recurse on the nested value(s)

Pattern matching in Haskell helps with both (1) and (3).

E.g., to compute the length of a list:

> length' :: [a] -> Int
> length' [] = 0
> length' (x:xs) = 1 + length' xs

E.g., more built-in functions:

> last' :: [a] -> a
> last' (x:[]) = x
> last' (_:xs) = last' xs
>
> 
> init' :: [a] -> [a] -- this function is almost never used (why?)
> init' (x:[]) = []
> init' (x:xs) = x : init' xs
>
>
> (+++) :: [a] -> [a] -> [a]
> [] +++ ys = ys
> (x:xs) +++ ys = x : xs +++ ys
>
>
> (!!!) :: [a] -> Int -> a -- the ! in its name is an implicit warning as to its inefficiency!
> (x:_) !!! 0 = x
> (_:xs) !!! n = xs !!! (n-1)
>
>
> cycle' :: [a] -> [a]
> cycle' xs = xs +++ cycle' xs
>
>
> reverse' :: [a] -> [a]
> reverse' [] = []
> reverse' (x:xs) = reverse' xs +++ [x] -- is there a more efficient way?
>
>
> zip' :: [a] -> [b] -> [(a,b)]
> zip' _ [] = []
> zip' [] _ = []
> zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
>
>
> take' :: Int -> [a] -> [a]
> take' 0 _ = []
> take' _ [] = []
> take' n (x:xs) = x : take' (n-1) xs
>
>
> drop' :: Int -> [a] -> [a]
> drop' 0 xs = xs
> drop' _ [] = []
> drop' n (x:xs) = drop' (n-1) xs
>
>
> elem' :: Eq a => a -> [a] -> Bool
> _ `elem'` [] = False
> x `elem'` (y:ys) = if x == y then True else x `elem'` ys
>
>
> break' :: (a -> Bool) -> [a] -> ([a], [a])
> break' _ [] = ([],[])
> break' p l@(x:xs) | p x = ([], l)
>                   | otherwise = let (ys, zs) = break' p xs
>                                 in (x:ys, zs)
>
>
> words' :: String -> [String]
> words' [] = []
> words' l@(c:cs) | isSpace c = words' cs
>                 | otherwise = let (w, ws) = break' isSpace l
>                               in w : words' ws
