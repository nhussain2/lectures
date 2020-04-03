% CS 340: Programming Paradigms and Patterns
% Lect 10 - Some Monads
% Michael Lee

> module Lect.Lect10Complete where
> import Data.List
> import Data.Maybe

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
>   fmap f (Logger x l) = Logger (f x) l

When combining `Logger` values as applicative instances, we should combine the
log messages found in both `Logger`s:

> instance Applicative Logger where
>   pure x = Logger x []
>   (Logger f l1) <*> (Logger x l2) = Logger (f x) (l1 ++ l2)

Finally, let's define the monad instance:

> instance Monad Logger where
>   (Logger x l) >>= f = let (Logger y l') = f x
>                        in Logger y (l ++ l')

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


State monad
-----------

Though Haskell doesn't allow for stateful functions, we can simulate the idea by
defining functions that take an input state and use it to compute a value,
returning that alongside an updated state. 

The `State` type represents just such a function:

> data State s a = State { runState :: s -> (s, a) }


We can define stateful functions that represent stack manipulations, where the
state is represented as a list and the computed value depends on the stack
operation semantics:

> pop :: State [a] a
> pop = State $ \(x:xs) -> (xs, x)

> push :: a -> State [a] ()
> push x = State $ \xs -> (x:xs, ())

> peek :: State [a] a
> peek = State $ \l@(x:xs) -> (l, x)


We can now run these operations like so on input lists:

    runState pop [1..10]

    runState (push 5) [1..10]

    runState peek [1..10]


More intriguingly, we can chain them, like this:

    let s1 = []
    in let (s2, _) = runState (push 5) s1
       in let (s3, _) = runState (push 7) s2
          in let (s4, x) = runState pop s3
             in let (s5, y) = runState pop s4
                in x+y

But this is really, really ugly. Monads exist to help us get rid of all the
manual chaining. So let's make a Monad out of State!

---

Start with Functor:

> instance Functor (State s) where
>   fmap f st = State $ \s -> let (s', x) = runState st s
>                             in (s', f x)

Then Applicative:

> instance Applicative (State s) where
>   pure x = State $ \s -> (s, x)
>   stf <*> stx = State $ \s -> let (s', f)  = runState stf s
>                                   (s'', x) = runState stx s'
>                               in (s'', f x)

And finally Monad:

> instance Monad (State s) where
>   st >>= f = State $ \s -> let (s', x) = runState st s
>                            in runState (f x) s'


Here's how we can use the State monad to chain together stack operations:

> stackArith :: State [Int] ()
> stackArith = do
>   w <- pop
>   x <- pop
>   let wx = w * x
>   y <- pop
>   z <- pop
>   let yz = y * z
>   push $ wx + yz

---

If we define a few functions that let us treat lists like mapping structures:

> get :: Eq a => a -> [(a,b)] -> b
> get x ((k,v):kvs) | x == k = v
>                   | otherwise = get x kvs

> put :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
> put x y [] = [(x,y)]
> put x y ((kv@(k,_)):kvs) | x == k = (k,y):kvs
>                          | otherwise = kv : put x y kvs


We can write stateful functions that use them to store and update "variable" values:

> var_get :: String -> State [(String, a)] a
> var_get v = State $ \s -> (s, get v s)

> var_put :: String -> a -> State [(String, a)] ()
> var_put v x = State $ \s -> (put v x s, ())


And now we can chain together what looks like a simple imperative program:

> quadRoots :: State [(String, Double)] (Double, Double)
> quadRoots = do
>   a <- var_get "a"
>   b <- var_get "b"
>   c <- var_get "c"
>   var_put "disc" $ b^2 - 4*a*c
>   disc <- var_get "disc"
>   var_put "r1" $ (-b - sqrt disc) / (2*a)
>   var_put "r2" $ (-b + sqrt disc) / (2*a)
>   r1 <- var_get "r1"
>   r2 <- var_get "r2"
>   return (r1, r2)

---

An example that starts to better illustrate the power of monads is in the
implementation of a tree-relabeling function.

Say we have a binary tree type:

> data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show


Let's write a function that "relabels" the values in a tree using values pulled
from a list. For example, if we use the list "abcdefg" to relabel the following
tree, we should get back the tree where 1 is replaced with 'a', 2 with 'b', 3
with 'c', etc.

> t1 :: Tree Int
> t1 = Node 1 
>        (Node 2 
>          (Leaf 3)
>          (Leaf 4))
>        (Node 5 
>          (Leaf 6)
>          (Leaf 7))


Here's the start of an attempt:

> relabel :: Tree a -> [b] -> Tree b
> relabel (Leaf x) (y:ys) = Leaf y
> relabel (Node x l r) (y:ys) = undefined -- how to write this?


Implementing the recursive case is somewhat painful, as we need to apply as many
list values as necessary to relabel the left subtree, then apply the remaining
to relabel the right subtree ... but to do this we need to keep track of how
many values we "use up" from the list while recursing downwards. Possible,
certainly, but ugly!

The State monad to the rescue. Note how, in the following code, the monad takes
care of maintaining its own internal state. 

> relabel' :: Tree a -> State [b] (Tree b)
> relabel' (Leaf x) = do l <- pop
>                        return $ Leaf l
> 
> relabel' (Node x l r) = do y <- pop
>                            l' <- relabel' l
>                            r' <- relabel' r
>                            return $ Node y l' r'


To relabel the tree above, we need simply do:

    runState (relabel' t1) "abcdefg"


Note that we could also have implemented relabel using the applicative style:

> relabel'' :: Tree a -> State [b] (Tree b)
> relabel'' (Leaf x) = Leaf <$> pop
> relabel'' (Node x l r) = Node <$> pop <*> relabel' l <*> relabel' r
