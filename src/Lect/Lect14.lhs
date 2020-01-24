% CS 340: Programming Paradigms and Patterns
% Lect 14 - Some Monads
% Michael Lee

> module Lect.Lect14 where

Some Monad Instances
====================

From here on, we'll be defining instances of the built-in Haskell functor
typeclasses (Functor, Applicative, Monad), so that "do" notation can be used
correctly (previously, we lucked out because the instances we defined were
already found in the Haskell library).

Sequencing
----------

In addition to return and bind, the Monad typeclass defined by Haskell includes
an additional method, >>, shown below:

  class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b

Remember that >>= is automatically invoked in do notation when encountering a
line that "extracts" the contents of a monadic value for use in the following
line(s). E.g.,

> _ = do
>   x <- Just 5
>   y <- Just 10
>   return (x + y) -- => 15

Is equivalent to:

> _ = Just 5 >>= \x ->
>     Just 10 >>= \y ->
>     return (x + y) -- => 15

But there are situations where, instead of manipulating the value contained by a
monad, what we really want to do is directly operate on the computational
context. Consider:

> _ = do
>   x <- Just 5
>   y <- Just 10
>   Nothing
>   return (x + y) -- => Nothing

The line containing Nothing in the do block doesn't extract anything from the
monadic value, so the bind operator isn't appropriate. However, we still need to
account for the presence of it --- this do block is interpreted as follows:

> _ = Just 5 >>= \x ->
>     Just 10 >>= \y ->
>     Nothing >>
>     return (x + y) -- => Nothing

As we can infer from its type, >> ignores the value contained in its first
monadic argument. It should, however, bind the monad to the one in the following
line (if there is one).

The default implemention of >> is : m >> k = m >>= \_ -> k --- this leads to the
following re-interpretation of the do block above:

> _ = Just 5 >>= \x ->
>     Just 10 >>= \y ->
>     Nothing >>= \_ ->
>     return (x + y) -- => Nothing

We'll see how this is useful in the next example!

A Simple Logging Monad
----------------------

> data Logger a = Logger {
>   logval :: a,
>   logmsg :: String
> } deriving (Show)
>
> instance Functor Logger where
>   fmap f (Logger x l) = Logger (f x) l
>
> instance Applicative Logger where
>   pure x = Logger x ""
>   (Logger f l1) <*> (Logger x l2) = Logger (f x) (l1 ++ ", " ++ l2)
>
> instance Monad Logger where
>   (Logger x l) >>= f = let (Logger y l') = f x
>                        in Logger y (l ++ ", " ++ l')

> loggedVal :: Show a => a -> Logger a
> loggedVal x = Logger x $ "Got " ++ show x
> 
> loggedOp :: Show a => String -> a -> Logger a
> loggedOp op x = Logger x $ "Performed " ++ op ++ " => " ++ show x

> logeg1 = do
>   x <- loggedVal 10
>   y <- loggedVal 5
>   loggedOp "Add" $ x + y

> logAppend :: String -> Logger ()
> logAppend l = Logger () l

> logeg2 = do
>   logAppend "Starting"
>   x <- loggedVal 10
>   logAppend "Doing something crazy"
>   y <- loggedVal 5
>   loggedOp "Add" $ x + y

State Monad
-----------

> data State s a = State { runState :: s -> (a,s) }
>
> instance Functor (State s) where
>   fmap f st = State $ \s -> let (x,s') = runState st s
>                             in (f x, s')
>
> inc :: a -> State Int a
> inc x = State $ \s -> (x, s+1)
>
> eg1 = do a <- inc 1
>          b <- inc 2
>          return (a+b)
>
> instance Applicative (State s) where
>   pure x = State $ \s -> (x,s)
>   stf <*> stx = State $ \s -> let (f,s') = runState stf s
>                                   (x,s'') = runState stx s'
>                               in (f x, s'')
>
> eg2 = (inc (*) <*> inc 2 <*> inc 3)
> 
> instance Monad (State s) where  
>    return x = State $ \s -> (x,s)  
>    st >>= f = State $ \s -> let (x,s') = runState st s
>                             in runState (f x) s'
>
> get :: State Int Int
> get = State $ \s -> (s,s)
>
> put :: Int -> State Int ()
> put x = State $ \s -> ((), x)

> pop :: State [Int] Int  
> pop = State $ \(x:xs) -> (x,xs)  
>   
> push :: Int -> State [Int] ()  
> push a = State $ \xs -> ((),a:xs) 


> stackArith :: State [Int] Int
> stackArith = do
>   push 1
>   push 2
>   push 3
>   a <- pop
>   b <- pop
>   push a
>   push b
>   push $ a * b
>   pop

