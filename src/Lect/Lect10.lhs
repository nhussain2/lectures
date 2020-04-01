% CS 340: Programming Paradigms and Patterns
% Lect 10 - Some Monads
% Michael Lee

> module Lect.Lect10 where

Some Monads
===========

Agenda:
  - List
  - Logger
  - State
  - IO


List monad
----------

Recall that the built-in applicative instance of the list type uses the
non-deterministic intepretation of the applicative `<*>` operator:

> noun_phrases = (++) <$> ["red ", "quick ", "fuzzy "] 
>                     <*> ["fox", "couch", "torpedo"]


The list monad instance is consistent with this interpretation:

    instance Monad [] where
      xs >>= f = concat [f x | x <- xs]

What do the following evaluate to?

    [1..10] >>= return

    [1..10] >>= \x -> replicate x x

    do x <- [1..10]
       y <- "hello"
       return (x,y)

    do article <- ["The", "A", "This"]
       adjective <- ["red", "quick", "fuzzy"]
       noun <- ["fox", "couch", "torpedo"]
       return $ article ++ " " ++ adjective ++ " " ++ noun

In some cases the use of bind can be replaced with pure functions and the `<$>`
and `<*>` operators. This is not always the case, though!


Logger monad
------------

The Logger monad is designed to provide a mechanism for attaching log messages
to values and computations. The log messages are automatically (via the
bind/sequence operators) combined and accumulated throughout sequences of
monadic operations.

We start by defining a type that encapsulates a polymorphic value and a list of
log messages:

> data Logger a = Logger {
>   loggerVal  :: a,
>   loggerMsgs :: [String]
> } deriving (Show)

As a functor, we should be able to apply functions to the value in a `Logger`:

> instance Functor Logger where
>   fmap = undefined

When combining `Logger` values as applicative instances, we should combine the
log messages found in both `Logger`s:

> instance Applicative Logger where
>   pure = undefined
>   (<*>) = undefined

Finally, let's define the monad instance:

> instance Monad Logger where
>   (>>=) = undefined

We need a few functions that produce `Logger` values:

> recordLog :: Show a => a -> String -> Logger a
> recordLog x s = Logger x [s]

> logVal :: Show a => a -> Logger a
> logVal x = recordLog x $ "Got " ++ show x
> 
> logOp :: Show a => String -> a -> Logger a
> logOp op x = recordLog x $ "Performing " ++ op ++ " => " ++ show x


What do the following evaluate to?

> logeg1 = return 5 >>= logVal

> logeg2 = do
>   a <- logVal 10
>   b <- logOp "Times 2" $ a*2
>   return b

> logeg3 = do
>   a <- logVal 5
>   b <- logVal 10
>   c <- logOp "Sub" $ a - b
>   d <- logOp "Square" $ c^2
>   return d


We may want functions to operate purely on log messages:

> logAppend :: String -> Logger ()
> logAppend l = recordLog () l


What do the following evaluate to?

> logeg4 = do
>   logAppend "Starting"
>   logAppend "Revving up"
>   logAppend "Warmed up"
>   return "Boom!"

> logeg5 = do
>   logAppend "Starting"
>   x <- logVal 10
>   logAppend "Revving up"
>   y <- logVal 20
>   logAppend "Warmed up"
>   z <- logOp "Add" $ x + y
>   return "Boom!"
