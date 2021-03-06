% CS 340: Programming Paradigms and Patterns
% Lect 04 - Lists
% Michael Lee

-- This Lect 04 was copy/pasted from Github's source code (raw file)

> module Lect.Lect04 where
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

          -- either an empty list[] or | it's an element followed by a list of things
    [a] = [] | a : [a]
    -- not a real type definition
    
-- recursive definition, one item and a list of the same type of item
Read as: A list containing instances of type 'a' ([a]) is either an empty
         list ([]) or an instance of type 'a' followed by ':' and a list
         containing instances of type 'a' ([a]).

Takeaways: 
  - a list is defined recursively, i.e., in terms of itself
  - the list type is polymorphic -- can hold any type
  - lists are homogeneous (all elements of a given list are of the same type)
-- have to be same type

Constructing lists
------------------

`[]` and `:` are examples of value constructors (aka data constructors); i.e.,
they are functions that return values of the associated type (in this case, the
polymorphic list type).

`[]` is called "empty list" and the `:` operator is called "cons":
Called 'cons' because it is constructing the list.

    [] :: [a] 

    (:) :: a -> [a] -> [a]
    -- function that takes a, takes list of a's and returns list of a's
    (:) 5 [] will give us [5] -- constructs list of single integer, 5
    cons 5 [] will give us [5] -- : is cons
    cons 3 (cons 5 []) will give us [3,5]
    -- takes element, and list of same type of element

We can call `:` in prefix form to build lists:

    (:) 1 [] will give us [1]

    (:) 1 ((:) 2 []) will give us [1,2]

    (:) 1 ((:) 2 ((:) 3 [])) will give us [1,2,3]

But we usually prefer to use `:` in infix form:
-- since : it is symbolic operator
    5 : [] will give us [5]

    1 : (2 : (3 : [])) will give us [1,2,3]

And `:`  is right-associative, so we can just write:

    1 : 2 : 3 : []
    right-most 'cons' eg. cons 3 : [] will get evaluated first

E.g., lists of integers:

    [] 

    100 : [] i

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
> replicate' 0 x = [] -- base case
> replicate' n x = x : replicate' (n-1) x 


-- replicate takes 2 arguments, number and a value - where value is what gets replicated
replicate 5 1 will give us [1,1,1,1,1]
or replicate 3 'x' will give us ["x","x","x"]

replicate 1 x = x : [] (using cons operator)

enumFromTo gives values in between starting and ending
eg. enumFromTo 5 10 gives us [5,6,7,8,9,10]
eg. enumFromTo 'c' 'g' give us 'cdefg'
-- successor of x, and y
-- not x+1 because it's enum not int, hence has successor which is next value in very general way (succ x) y
> enumFromTo' :: (Ord a, Enum a) => a -> a -> [a]
> enumFromTo' x y | x <= y = x : enumFromTo' (succ x) y 
>                 | otherwise = [] -- base case (if y is not greater than equal to x)
-- x y need to be compared to each other (Comparables)
>
> -- and now for some infinite lists
>
> ones :: [Int]
> ones = 1 : ones
-- 1 in front of list (consists of 1 in front of a list) etc.
> 
-- repeat arbitrary value a, get back list of a's
> repeat' :: a -> [a]
> repeat' x = x : repeat' x
>
-- don't specify the end, just keep going forever
-- start with value x, keep going 
> enumFrom' :: Enum a => a -> [a]
> enumFrom' x = x : enumFrom' (succ x) -- keep adding on successor

Note: to limit the number of values drawn from an infinite list, we can use
      `take` (we'll implement it later)

      take 5 ones would give us [1,1,1,1,1]
      take 5 (repeat' a) gives us "aaaaa"

      -- in a non-lazy language, infinite list would stop the program bc it can't evaluate any further, but take will work on this lazy language

      'drop' drops a certain number of values from a list
      drop 5 (enumFromTo 10 20) drops the first 5 values
      take 10 (drop 5 (infinite list)) would drop the first 5 values, then take 10 from the list after values have been dropped

Syntactic sugar
---------------

Instead of constructing lists with `:`, Haskell gives us syntactic shortcuts:

E.g., for simple itemized lists, [...]

    -- synctatic sugar       vs  what's going on
    [1,2,3,4,5,6,7,8,9,10]   ==  1:2:3:4:5:6:7:8:9:10:[] 

    [[1, 2, 3], [4, 5, 6]]   ==  (1:2:3:[]) : (4:5:6:[]) : []

    [(2, True), (1, False)]  ==  (2,True) : (1,False) : []


E.g., for lists of characters (string), "...":

    "hello world"       ==  'h':'e':'l':'l':'o':[]

    ["hello", "world"]  ==  ('h':'e':'l':'l':'o':[]) : ('w':'o':'r':'l':'d':[]) : []


E.g., for types that are instances of `Enum`, [I..J] and [I,J..K] and [I..]:

    [1..10]     ==  [1,2,3,4,5,6,7,8,9,10]

    ['a'..'z']  ==  "abcdefghijklmnopqrstuvwxyz"
    -- specify the first two values, can define skip 
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
-- python would be 2*x for etc.. using | as for
[2*x | x <- [1..5]]
[2,4,6,8,10]
<- is where x is coming from

> evens = [2*x | x <- [1..]]
>
-- get all x's of all numbers, apply filters where x mod 2 == 0
-- following, is the predicate
> evens' = [x | x <- [1..], x `mod` 2 == 0]
> 
-- 2 different source lists, building a list of lists
> sudokuBoxes = [[[r,c] | r <- rs, c <- cs] | rs <- ["ABC", "DEF", "GHI"],
>                                             cs <- ["123", "456", "789"]]
>
> integerRightTriangles p = [(a,b,c) | a <- [1..p], 
>                                      b <- [a..(p-a)],
>                                      let c = p-(a+b),
>                                      a^2 + b^2 == c^2]

-- can define new local variable inside list constructor
>
> factors :: Integral a => a -> [a]
> factors n = [f <- [1..n], n `mod` f == 0]
>
> cartesianProduct :: [a] -> [b] -> [(a,b)]
> cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]
-- cycle through all values of x, and all values of y

> concat' :: [[a]] -> [a]
> concat' ls = [x | l <- ls, x <- l]

-- x <- l (get each item from each list)
-- l <- ls (get each list within the list of lists )


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
    splitAt :: Int -> [a] -> ([a], [a])
    break :: (a -> Bool) -> [a] -> ([a], [a])

  - Class specific:

    elem :: Eq a => a -> [a] -> Bool -- checks if elem in list
    -- if we do, elem 'a' [1..10] - error - different type, will say True or False in list of same type
    maximum :: Ord a => [a] -> a 
    minimum :: Ord a => [a] -> a
    sum :: Num a => [a] -> a
    product :: Num a => [a] -> a
    lines :: String -> [String]
    words :: String -> [String]

    ord gets you the ASCII code of a character, like ord 'a' will give u its ASCII
    chr will give you character of the ASCII code

Note: many of these functions operate on a type class that includes lists and
      other recursive data types (We'll see how this works later.)


List processing functions
-------------------------

-- Pattern matching

`[]` and `:` can be used to pattern match against lists (we'll see that all
value constructors can be used for pattern matching). So the first three basic
operations are trivial to re-implement:

> head' :: [a] -> a -- takes list of a's, returns a
> head' (x:_) = x -- given an input, that input consists of an element in front of a list 
-- eg. (x:xs) x in front of some x's (parenthesis is necessary, value constructor for pattern match)

-- head' (5:[]) will be 5
-- if head' 5:4:3:2[], it would be 5
-- cons is right associative, so 5:(4:(3:(2:[]))) 
-- head' [5,4,3,2] will also be 5
>
> tail' :: [a] -> [a]
> tail' (_:xs) = xs
-- get everything but the head
-- tail returns list, head returns element of a list
-- tail [1] would give us []
-- tail (1:[])
> 
> null' :: [a] -> Bool
> null' [] = True
> null' _  = False
-- returns True if empty list, otherwise False


But most other list-processing functions need to potentially extract multiple
values from the list. A common technique for doing this is structural recursion.


-- Structural recursion

Structural recursion loosely describes a pattern for writing functions that
handle recursively defined data types (like the list). When called with a value
of such a type, a structurally recursive function will:

  1. start by determining how the value was constructed

  2. if the value is not a recursive instance of the data type, simply process
     its immediate contents (base case)

  3. if the value is a recursive instance of the data type, "deconstruct" it to 
     process its immediate contents, then recurse on the nested value(s) (recursive case)

Pattern matching in Haskell helps with both (1) and (3).

E.g., to compute the length of a list:

-- could also add, length([x]) = 1
-- or length(x:[]) = 1
-- however the second line already takes care of that case
> length' :: [a] -> Int
> length' [] = 0 -- base case (length of empty list is 0)
> length' (_:xs) = 1 + length' xs -- take list apart, such that there is a value and a list, recurse on list

-- recursive constructor in a list is the cons operator
-- start and list -> it's recursive

E.g., more built-in functions:

-- gives back last element
> last' :: [a] -> a
> last' [] = error "Empty list" -- base case, when the list is empty
> last' [x] = x -- case where one element is present, return the only element (also like last' (x:[]) = x)
> last' (_:xs) = last' xs
>
-- ++ function, list concatenation, it will add both lists
--(+++) :: [a] -> [a] -> [a]
--(+++) [] [] = [] -- [] +++ [] = []
--(+++) xs [] = xs
--[] +++ ys = ys

-- can write it now as 
> xs +++ [] = xs
> [] +++ ys = ys
> (x:xs) +++ (y:ys) = x : (xs +++ (y:ys)) -- x will be first value, and then everything else, recursed

-- 'index of' operator
> (!!!) :: [a] -> Int -> a -- the ! in its name is an implicit warning as to its inefficiency!
> [] !!! _ = error "index too large" -- base case
> (x:_) !!! 0 = x -- first element
> (x:xs) !!! n = xs !!! (n-1) -- n-1th index of xs (the rest of the values) recursive case
>
-- takes a list and returns the reversed list
> reverse' :: [a] -> [a]
> reverse' [] = [] -- base case, empty list
-- reverse' [x] = [x] -- single element base case, not necessary, as it would be satisfied in below function
> reverse' (x:xs) = reverse' xs  +++ [x] -- reverse rest of list and concat to list of x (turn single element into list)
-- VERY INEFFICIENT! 
>
>
-- draws from the beginning of a list
> take' :: Int -> [a] -> [a]
> take' _ [] = [] -- take anything from empty list, base case
> take' 0 _ = [] -- base case, take 0 from anything
> take' n (x:xs) = x : take' (n-1) xs -- take n-1 from the rest of the list and x from the beginning of the list
>

-- give an integer and a list, gives a tuple of 2 lists
-- split at 0, [], rest of list on other side
-- if bigger than list, all of values, []
-- for any index, empty list on both sides
> splitAt' :: Int -> [a] -> ([a], [a])
> splitAt' _ [] = ([],[]) -- base case, empty list
> splitAt' 0 xs = ([], xs) -- base case, 0 with list, empty list in front
> splitAt' n (x:xs) =  let (ys, zs) = splitAt' (n-1) xs
                       in (x:ys, zs)
-- got a let function to pattern match, recurse for n-1
-- deconstructing result and putting it back into return value
-- recursive call gives a common pattern
>
-- function that takes a value and returns a boolean (predicate)
-- making a decision about that value (predicate)
-- takes a list of as as well, returns tuple
-- breaks on first value that is true in the condition (matches the predicate and chops the list there)
> break' :: (a -> Bool) -> [a] -> ([a], [a])
> break' = _ [] = ([],[]) -- base case
> break' p(x:xs) = if p x -- if predicate is true on x
                   then ([], x:xs) -- break at start of x
                   else let (ys, zs) = break' p xs in (x:ys, zs)
                   -- else, recurse using pattern matching again
>
-- Let's clean it up now!
> break'' :: (a -> Bool) -> [a] -> ([a], [a])
> break'' = _ [] = ([],[]) -- base case
> break'' p l@(x:xs) | p x = ([], l) -- break at start of x
                     | otherwise = let (ys, zs) = break' p xs 
                                   in (x:ys, zs)

-- get back a list of strings, without spaces
> words' :: String -> [String]
> words' "" = [] -- base case (empty string gives empty list)
-- function that eats up all the spaces
> words' (' ':cs) = words' cs -- if space, eat it up, leave rest of character
> words' l@(c:cs) = let (w, ds) = break' isSpace l   
                  in w : words' ds -- on the rest of the words now

E.g., the Caesar cipher is an encryption scheme that takes a plain text input
string P and a shift value N, and produces an encrypted version of the string
by replacing each letter in P with one N letters away in the alphabet (wrapping
around, if needed).

For the string "HELLO WORLD" and a shift value of 5:
-- shifts characters to the right by 5, if it reaches end of alphabet, you loop to the beginning of the alphabet
  Plain:      H E L L O  W O R L D
  Encrypted:  M J Q Q T  B T W Q I

To implement the Caesar cipher, we need to be able to convert characters from/to
their corresponding ASCII codes. `ord`/`chr` do this. `isLetter` can be used to
determine if a character is a letter. We'll convert all letters to uppercase
for simplicity with `toUpper`.

-- takes n, and strings (n is the shift value)

> caesar :: Int -> String -> String
> caesar _ "" = "" -- empty string gives empty string
> caesar 0 s = s -- base case, shift of 0 gives the same string
> caesar n (c:cs) = (if isLetter c then encript (toUpperc) else c) : caesar n cs 
-- entire if else evaluates to the character which we then stick it to
>      where encrypt c = n2c ((c2n c + n) `mod` 26)
>            n2c n = chr (n + ordA) -- convert number to char
>            c2n c = ord c - ordA -- convert char to number
>            ordA = 65
-- n2c is number to char, number to c2n

-- deal with a single element, then process the rest recursively
-- uppercase all letters
-- toUpper takes lower case letter and gives an uppercase
-- implement top down? 
-- isLetter can check for whether its a letter
-- to wrap around, for ord 'X' + 10, it will go onwards
-- hence, we need to get back to 26
-- (ord 'X' - ord 'A'  + 10) mod 26 + ord A
-- get remainder and use that if exceeding value