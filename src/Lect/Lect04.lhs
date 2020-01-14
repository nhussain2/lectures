% CS 340: Programming Paradigms and Patterns
% Lect 04 - Functions
% Michael Lee

> module Lect.Lect04 where

Functions
=========

Defining Functions
------------------

* Functions are typically defined with `=`

* A function definition starts with its name, is followed by its formal
  parameters, then `=`, then an *expression* that is to be evaluated to
  determine the result of the function

* Note that a function is just another type of value that we bind to a
  variable (its name) using `=` --- we can also pass functions as values
  to other functions and return them as results. We say that Haskell (and
  all functional languages) have "first-class functions".

> 
>
>

Function Type Declarations
--------------------------

Though Haskell can infer types for us, it is good practice to always explitly
declare types for all our functions.

> -- declare the types for the following:
>
> 
> nand a b = not (a && b)
>
> 
> sum x y = x + y
>
>
> fst' (x,y) = x
> 
>
> disc (a,b,c) = b^2-4*a*c
>
> 
> quad_roots a b c = ((-b+(sqrt d))/(2*a), (-b-(sqrt d))/(2*a))
>   where d = disc (a,b,c)
>
>
> comp a b c d = if a == b then c else d
>
> 
> sum_or_diff a b = if a > b then a - b else a + b
>


