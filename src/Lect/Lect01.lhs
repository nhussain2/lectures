% CS 340: Programming Paradigms and Patterns
% Lect 01 - Introduction to Haskell
% Michael Lee

> module Lect.Lect01 where
> import Data.List

Introduction to Haskell
=======================

Agenda:
  - working with literate source files
  - general development workflow and tools
  - notable (& maybe surprising) language features
  - indentation and layout rules


Literate Haskell
----------------

Regular Haskell source code filenames end in ".hs" --- all your assignments
will use this extension --- but they can also end in ".lhs", which denotes a
"literate" Haskell source file. In a literate source file, all text defaults
to being a comment; lines of source code must start with the ">" character,
and must also be separated from other text by at least one empty line.

All lecture notes will be provided as literate source files. Here's what code
would look like in a literate source file:

> welcome = putStrLn "Welcome to CS 340!"


Development workflow and tools
------------------------------

You'll need to switch frequently between multiple tools and platforms to
do your work in this class. They include:

- Git, a version control system

- GitHub, a Git hosting service

- Stack, a tool you'll use to install the Haskell compiler and libraries,
  and compile and test your code

- a source code editor with Haskell support --- I recommend Visual Studio Code
  with the Haskelly plugin unless you strongly prefer a different editor and
  are willing to figure out how to set it up on your own


Notable (& maybe surprising) language features
----------------------------------------------

1. Strong static typing: Every expression or variable has a type associated with
                         it that doesn't change, and is rigidly enforced by the
                         compiler. Haskel programs are type-safe; i.e., there
                         will never be run-time type related errors!


2. Type inference: The compiler can figure out the types of many things, so that
                   we don't have to expliclty label them.

> -- use the :t GHCi command to get the type of each of the below
>
> mysteryVar1 = 123 `mod` 8
>
> mysteryVar2 = words "hello how are you?"
> 
> mysteryFn1 = (^2)
>
> mysteryFn2 = length . words -- composition of two functions, composing the function 'length' with the function 'words'
-- length finds the length of a list
-- :t mysteryFn2 (type) will be String -> Int (takes String, returns Int)
-- mysteryFn2 "hello how are you" --> 4
> 
> mysteryFn3 f s = [(abs $ f $ length w, w) | w <- words s]


3. Purely functional: Once variables are bound to values they cannot be changed
                      (i.e., variables are immutable).

> boundVar = 10
> -- boundVar = 20 -- error!
-- ghci, if you recompile, you can change the value because it's rerun. In source file though, it will give a multiple declarations error.
 

4. Lazy evaluation: Expressions (e.g., function calls) are not evaluated until
                    their results are strictly needed. Unevaluated computations,
                    called "thunks", are maintained in a graph.

-- cannot evaluate error or undefined in Haskell, program breaks

> possiblyTragic c = let e = error "Eeek!" -- e is set to error "Eeek!"
>                        u = undefined -- u is set to undefined, both e, u are dangerous - evaluating them will break program
>                    in case c of 'e' -> e -- if c is e, evaluate e variable
>                                 'u' -> u -- if u, then evaluate u variable
>                                 otherwise -> "safe" -- otherwise evaluate "safe"

-- this is an instance where laziness comes in handy.

-- possiblyTragic 'e' gives error Eeek!
-- possiblyTragic 'u' gives error
-- possiblyTragic 'a' gives safe

-- takes a character :t Char -> [Char] takes character, returns list of characters (String)

>
-- div is for integer division: 10 div 3 = 3, 10 div 0 = error (no way to define infinity)

> safeDiv x y = let q = x `div` y -- q = x div y
>               in if y == 0 -- if y is zero, return 0 otherwise return q (which is the actual division)
>                  then 0 -- feels backwards bc q has already been computed???
>                  else q
-- safeDiv 10 0, 0
-- lazy, never evaluates because it sees that y is 0, hence returns 0
-- type for safeDiv? 

5 . Order-independence: The order bindings appear in code doesn't matter.

> w = x + 2 
> x = w - 5
-- what's x? no answer, it's infinite (recurses forever)
-- Haskell throws no error but compiles a recursion that doesn't end
>
> evenN 0 = True 
> evenN n = oddN (n-1)
>
> oddN 0 = False
> oddN n = evenN (n-1)

-- mutually recursive functions

6. Concise

   - Small language with few keywords:

       case  class  data   deriving  do   else  if
       import  in  infix  infixl  infixr  instance
       let  module  newtype  of  then  type  where

   - Declarative vs. Imperative style!

> palindromes = sortOn snd
>               . map (\w -> (w,length w)) -- map every string onto a tuple, string itself and the length of it
>               . filter (\s -> reverse s == s) -- filtered using a lambda function
>               . words 


Indentation & Layout rules
--------------------------

Haskell supports the use of semicolons and curly braces for delineating
and separating blocks of code, but they are rarely used. Instead, we prefer 
to take advantage of *layout rules* that use indentation to group and separate 
code. 

The "golden rule" for Haskell indentation is:

   Code which is part of some expression should be indented further in 
   than the beginning of that expression.

In addition, all expressions that belong to the same group must be left-aligned
to the same column. The "do", "let", "where", and "of" keywords indicate the
start of group.

> doGuessing num = do
>   putStrLn "Enter your guess:"
>   guess <- getLine
>   case compare (read guess) num of
>     LT -> do putStrLn "Too low!"
>              doGuessing num
>     GT -> do putStrLn "Too high!"
>              doGuessing num 
>     EQ -> putStrLn "You Win!"

-- function that does IO

Read the [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell/Indentation)
for a briefer on the layout rule, and the 
[Haskell language report](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-210002.7)
for all the gory details.



Notes:
-- :r - reloads source file after changing it 
-- need to leave space below and above program line (starting with '>')
-- comment starts with 2 dashes
-- LHS - literate source file 
-- :q - quits out of ghci
-- String = [Char] --> String is a list of characters
-- words? Function that takes a string, returns a list of strings
-- words :: String -> [String]




