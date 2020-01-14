% CS 340: Programming Paradigms and Patterns
% Lect 03 - Types and Typeclasses
% Michael Lee

> module Lect.Lect03 where

Types and Typeclasses
=====================

Simple Types and Operations
---------------------------

* Haskell has predefined types and operators (functions) for numbers, Booleans,
  Chars, Strings, and lots more --- ["Prelude"][1] is the module that Haskell
  imports by default into all programs which defines all the standard
  types/functions

* The type of every expression is known at compile time, so "badly-typed"
  expressions are automatically reported (and don't compile!)

* The `:type` (`:t`) and `:info` (`:i`) ghci commands let us inspect types
  interactively

[1]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html

> a = 10
> b = 2^1000 -- Integer type has unlimited precision
> c = (True || False) && True
> d = 1000 == 1001
> e = 1000 /= 1001 -- note: weird not-equal-to

* Operators are just functions whose names start with non-letters, and are
  used in infix form (e.g., `+`)

  * Operators can be used in prefix form if we place them in parentheses

* Functions are called in prefix form, where the function name precedes
  arguments being passed to it --- parentheses are not needed (unless
  explicitly controlling precedence or creating tuples)

  * Functions can be used in infix form if we place them in backticks (``)

>
>
>

* The type specification for a function looks like this:

      type1 -> type2 -> ... -> typeN

  * Note: `->` associates to the right (how to parenthesize the above?)

  * This tells us that a function of *N* arguments can be viewed as a function
    of *one* argument that returns a function of *N-1* arguments!

>
>
>

Basic Types
-----------

- Bool    - True/False
- Char    - Unicode character
- Int     - 64 bit signed integer
- Integer - arbitrary-precision integer
- Float   - 32-bit IEEE single-precision floating point number
- Double  - 64-bit IEEE double-precision floating point number
- Tuple   - finite (i.e., of a given arity) sequence of different types

Note, all types have capitalized names!

Note: `:t` can be used to ask for the type of any expression

< -- try out
< :t True
< :t False || True
< :t 'a'
< :t 5
< :t sqrt 5


Function Types
--------------

A function is a mapping (->) from one type (the domain) to another type (the
range).  

< -- e.g.,
< not  :: Bool -> Bool
< even :: Int  -> Bool

A function of multiple arguments can be implemented in one of two ways:

1. A function that takes a tuple of the requisite types

< foo :: (Int, Bool, Char) -> Int

2. A *curried* function

< foo :: Int -> Bool -> Char -> Int

   Note: (->) associates right-to-left, so, equivalent to:

< foo :: Int -> (Bool -> (Char -> Int))

   I.e., foo is a function which takes an Int and
         returns a function which takes a Bool ...

   Function application (space) associates left-to-right, so:

< foo 5 True 'a'

   is equivalent to:

< (((foo 5) True) 'a')

Inspect the types of `id`, `const`, `fst`, and `snd`. Discuss.


Polymorphic Functions
---------------------

In the type `a -> b`, `a` and `b` are *type variables*. Since they are
unqualified, they can be replaced with any type! A function whose type
declaration contains a type variable is a *polymorphic* function.

Since an unqualified type variable says nothing about its actual type,
you can't do much with it (why?)

But this means the type of a polymorphic function is usually very helpful
in determining what it does!

e.g., what do `id`, `const`, `fst`, `snd` do?

Inspect the types of `==`, `+`, `show`, `read`


Overloaded Types and Type Classes
---------------------------------

< (+) :: Num a => a -> a -> a

Above, `Num a` is a *class constraint*, implying that the actual type of
type variable `a` must belong to the *type class* `Num`.

A type that contains a class constraint is called *overloaded*.

A *class* is a collection of types that support a set of overloaded
functions called *methods*. Each type belonging to a given class is called
an *instance* of that class.

`:i` gives us information on both types and classes.

< -- try out
< :i Int
< :i Integral
< :i Num
< :i Eq
< :i Ord
< :i Show
< :i Read
