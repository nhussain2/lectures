% CS 340: Programming Paradigms and Patterns
% Lect 02 - Introduction to Haskell
% Michael Lee

> module Lect.Lect02 where

Introduction to Haskell
=======================

Agenda:

- working with literate Haskell source files
- general development workflow with Stack and GHCi
- notable features and characteristics of the language
- on whitespace sensitivity


Literate Haskell
----------------


Development workflow
--------------------


Notable features
----------------

1. Strong static typing

2. Purely functional

3. Lazy evaluation

4. Concise

   - Functional style lends itself to concision. I.e., declarative "what"
     to do instead of imperative "how" to do it.

   - Small language with few keywords:

       case  class  data   deriving  do   else  if
       import  in  infix  infixl  infixr  instance
       let  module  newtype  of  then  type  where


Whitespace sensitivity
----------------------



Bindings and Purity
-------------------

* In Haskell, once we bind a value to a variable, we can't change that
  binding (though new bindings may shadow existing ones in containing
  scopes -- e.g., local variables shadowing global ones)

* Bindings may be established using `=` or when we call functions with
  parameters (which are bound to local variables)

* Variable names must start with a lowercase letter

> iAmAValidVariableName = 1
> -- IAmNotAValidVariableName = 2

* Other (potentially surprising) facts:

  * The order of bindings in a Haskell program is not important

  * The value of a binding is only evaluated when needed

  * A symbol being bound is in scope with its own definition

> w = x
> x = w
> y = y
> z = error "Eek!"
> u = undefined
