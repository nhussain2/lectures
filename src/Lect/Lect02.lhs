% CS 340: Programming Paradigms and Patterns
% Lect 02 - Types and Type Classes
% Michael Lee

> module Lect.Lect02 where
> import Data.Char

Types and Type Classes
======================

Agenda:
  - Types
  - Basic Types
  - Function types
  - Function application
  - "Operators"
  - Polymorphic functions 
  - Type Classes


Types
-----

A *type* defines a collection of values. -- may be infinite or very small collection, int type, infinite (-infinity to infinity)
-- characters, not as infinite

  - `e :: T` means that expression `e` (when evaluated) has type `T`
  -- syntax for declaring a type
  -- 'e :: T', expressinon 'e' has type 'T'
  -- 1 + 2 :: Int

  - all type names start with a capital letter -- hard and fast rule

Haskell has a small number of builtin types, and the "Prelude" module (which is
imported by default into all Haskell source files) defines other standard types.


Basic types
-----------

  - Bool    - True/False
  - Char    - Unicode character
  - Int     - 64 bit signed integer
  - Integer - arbitrary-precision integer
  - Float   - 32-bit IEEE single-precision floating point number -- rarely used by us
  - Double  - 64-bit IEEE double-precision floating point number -- rarely used by us
  - Tuple   - finite (i.e., of a given arity) sequence of zero or more types -- finite sequence of values, each value has a specific type

(At GHCi, we can use `:t` to ask for the type of any expression.)

What are the types of the following?

    True -- Bool
    'a' 
    5
    1.5
    ('b', False, 'c') -- Type of this tuple? Char Bool Char, sequence of 3 values 
    -- ('b', False, 'c;) :: (Char, Bool, Char)
    ()
    (True)
    (1, 2, 3, True)

    ---
    > foo :: (Char, Bool, Bool)
    foo = ('b', False, 'c') -- if running, it would fail as c is not a Bool
    -- tuple does not give the list/number of values but also the TYPE of each value within it.


Function types
--------------

A function is a mapping from one type (the domain) to another type (the range).
-- the two types may be the same as well as different
For a function that maps type T1 to type T2, we specify its type as `T1 -> T2`

Some functions:

    not  :: Bool -> Bool -- changes from T to F or F to T, hence maps from Boolean to Boolean 

    isDigit :: Char -> Bool


A function of multiple arguments can be implemented in different ways:

  1. A function that takes a tuple of the requisite types, e.g.,

         foo :: (Bool, Char) -> Int

  2. Functions that return other functions, e.g.,

         foo :: Bool -> (Char -> Int) 
         --     function that returns a function (that takes Char and returns Int)
         -- Bool -> Char -> Int (arrow operators go from right to left)
         -- type for (foo True)? returns function that returns (Char -> Int)
         -- type for (foo True 'a')? returns Int 
         
         -- function just takes one argument, one at a time

         -- function can be partially applied
         -- map (+1) [1,2,3,4,5]
         -- returns [2,3,4,5,6]

         -- n = map(+1)
         -- n waits for list, n [1,2,3]
         -- [2,3,4]

     We interpret this type as a function which takes a `Bool` and returns
     another function which takes a `Char` and returns an `Int`.

     Here's a function of three `Int` arguments:

         bar :: Int -> (Int -> (Int -> Int))

     We call functions that take arguments one at a time like this "curried"
     functions. This is the default in Haskell, so the `->` in type declarations
     associates to the right, which means we can just write:

         bar :: Int -> Int -> Int -> Int
         -- bar :: Int -> (Int -> (Int -> Int))

         -- right arrow operator is right associative
         -- if a bunch of arrows in a row, we apply right most first

         -- left associative:
         -- ((Int->Int)->Int)->Int

Function application
--------------------

Function application simply requires placing a space between a function name and
its argument(s), e.g.,

    not True -- no paranthesis bw function and argument, you just put a space

    isDigit '9'


Function application associates left-to-right, so 

    foo 5 True 'a'
    -- foo :: Int -> Bool -> Char

is equivalent to:

    (((foo 5) True) 'a')

-- left associative, left most space happens first
-- foo is applied to 5, which is applied to True, which is applied to 'a'
-- (foo 5) becomes a function
-- ((foo 5) True) then becomes function
-- (((foo 5) True) 'a') is then the function

Note that this is in keeping with function currying --- each function
application results in another function, that is then applied to an additional
argument to obtain another function ...

We will be making use of *partial function application* quite a bit!


"Operators"
-----------

Operators are just functions whose names start with non-letters, and are used
in infix form (e.g., `13 + 25`)

-- :t (+) ? operator, takes two numbers and evaluates to a number
-- :t (&&) :: Bool -> Bool -> Bool (takes 2 Bools, returns a Bool) AND

  - Operators can be used in prefix form if we place them in parentheses

        (&&) True False -- True && False ? 

        (+) 13 25 -- 13 + 25

        (^) 2 10 -- 2^5
        -- (2^) 5 = 32
        -- (^5) 2 = 32
        -- :t (-2)

  - Functions can be used in infix form if we place them in backticks (``)

        20 `mod` 3 -- same as mod 20 3

        36 `gcd` 27


Polymorphic functions
---------------------

Some functions are type-agnostic; i.e., they don't care about the types of some
of their arguments. E.g., consider a function that takes a tuple and returns 
the first element.

Such functions are still well-typed, but their types change (morph) according to
their specific argument types. We call these functional *polymorphic functions*,
and their type declarations contain *type variables*.

E.g., a function which takes a two-tuple and returns the first element has type:

    fst :: (a, b) -> a
    -- a and b are type variables, not concrete types

    foo :: (a, b, c) -> c
    -- tuple is fixed in size

    foo :: (a, Int, c) -> c
    -- second tuple item is type Int, but the other 2 are unknown types
    -- if you call second item with any other thing other than Int, it will give error

Since an unqualified type variable says nothing about its actual type, you can't
do much with the value of the corresponding argument.

But the type of a polymorphic function can actually be quite helpful in
determining what it does!

e.g., what do `snd`, `id`, `const`, do, based on their types?

snd :: (a,b) -> b
-- Gives the second item's type
id :: a -> a
-- Returns the type of itself
const :: a -> b -> a
-- Returns the type of the first element

e.g., try to decipher the types of `.` and `until`

(.) :: (b->c)->(a->b) -> a -> c
-- first argument (b->c)
-- second argument (a->b)

-- essentially, a -> b -> c
-- (b->c) . (a->b) compose together 

until :: (a->Bool) -> (a->a) -> a -> a
-- a function that takes a function that determines when we're done
-- a function that computes next value based on initial value
-- computes initial value

-- all are a because they're not same values, just same type

Type Classes (aka Classes)
--------------------------

Just as a type is a collection of related values, a type *class* is a collection
of related types. A class defines functions (known as methods) that are
supported by all instances (i.e., types) of that class

-- type is an instance of a type class

Some common classes, their methods, and instances:

    class Eq a where
      (==) :: a -> a -> Bool
      (/=) :: a -> a -> Bool -- /= not equal, like !=

      instances: Int, Integer, Float, Double, Char, Bool
  -- class that supports equality testing
  -- 

    class Ord a where
      compare :: a -> a -> Ordering
      (<) :: a -> a -> Bool
      (<=) :: a -> a -> Bool
      (>) :: a -> a -> Bool
      (>=) :: a -> a -> Bool
      max :: a -> a -> a
      min :: a -> a -> a

      instances: Int, Integer, Float, Double, Char, Bool


    class Num a where
      (+) :: a -> a -> a
      (-) :: a -> a -> a
      (*) :: a -> a -> a
      negate :: a -> a
      abs :: a -> a
      signum :: a -> a
      fromInteger :: Integer -> a

      instances: Int, Integer, Float, Double


Polymorphic function type declarations can also include *class constraints*, 
which indicate that constrained type variables must be instances of specific 
classes.

E.g., the following type declaration says that type variable `a` must be an
instance of the `Num` class:

    subtract :: Num a => a -> a -> a


Inspect the types of `^`, `show`, `read`, `length`. (At GHCi, you can use `:i` 
to get more information about types, classes, methods, and class instances.)

(^) :: (Integral b, Num a) => a -> b -> a
-- Takes 2 arguments, b has to be Integral, a has to be Num (Num and Integral are Type constraints)

show  

read :: Read a => String -> a
-- takes string, returns arbitrary type
-- you have to define the type, like
-- read "1" :: Int (will return 1)

length :: Foldable t => t a -> Int
-- Function that gets applied to a type which has to give back another function
-- t is a type constructor

-- Foldable, type that wraps another type

-- Used for a data structure