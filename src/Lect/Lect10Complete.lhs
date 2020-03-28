% CS 340: Programming Paradigms and Patterns
% Lect 10 - Some Monads
% Michael Lee

> module Lect.Lect10Complete where

Some Monads
===========

Agenda:
  - List
  - Logger
  - State
  - IO


List Monad
----------

Just as with the list applicative, the list monad supports nondeterministic
programming via its bind operator.

    instance Monad [] where
      xs >>= f = concat [f x | x <- xs]

Consider:

noun_phrases = do
  article <- ["The", "A", "This"]
  adjective <- ["red", "quick", "fuzzy"]
  noun <- ["fox", "couch", "torpedo"]
  return $ intercalate " " [article, adjective, noun]

Logger Monad
------------

> data Logger a = Logger {
>   logVal  :: a,
>   logMsgs :: [String]
> } deriving (Show)
>
> instance Functor Logger where
>   fmap f (Logger x l) = Logger (f x) l
>
> instance Applicative Logger where
>   pure x = Logger x []
>   (Logger f l1) <*> (Logger x l2) = Logger (f x) (l1 ++ l2)
>
> instance Monad Logger where
>   (Logger x l) >>= f = let (Logger y l') = f x
>                        in Logger y (l ++ l')

> recordLog :: Show a => a -> String -> Logger a
> recordLog x s = Logger x [s]

> loggedVal :: Show a => a -> Logger a
> loggedVal x = recordLog x $ show x
> 
> loggedOp :: Show a => String -> a -> Logger a
> loggedOp op x = recordLog x $ "Performed " ++ op ++ " => " ++ show x

> logeg1 = do
>   x <- loggedVal 10
>   y <- loggedVal 5
>   loggedOp "Add" $ x + y

> logAppend :: String -> Logger ()
> logAppend l = recordLog () l

> logeg2 = do
>   logAppend "Starting"
>   x <- loggedVal 10
>   logAppend "Doing something crazy"
>   y <- loggedVal 5
>   loggedOp "Add" $ x + y


State Monad
-----------

> newtype State s a = State { runState :: s -> (s,a) }

> instance Functor (State s) where
>   fmap f m = State $ \s -> let (s',x) = runState m s in (s',f x)
>
> instance Applicative (State s) where
>   pure x = State $ \s -> (s,x)
>
>   mf <*> mx = State $ \s -> let (s',f) = runState mf s
>                             in runState (f <$> mx) s'
>
> instance Monad (State s) where
>   m >>= f = State $ \s -> let (s',x) = runState m s
>                           in runState (f x) s'

> pop :: State [Int] Int  
> pop = State $ \(x:xs) -> (xs, x)
>   
> push :: Int -> State [Int] ()  
> push a = State $ \xs -> (a:xs,()) 
>
> rot :: State [Int] ()
> rot = State $ \(x:xs) -> (xs ++ [x], ())

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


IO Monad
--------

  newtype IO a = IO { runAction :: RealWorld -> (RealWorld, a) }

Cont monad
----------

data Cont r a = Cont { runCont :: (a -> r) -> r }
instance Functor (Cont r) where
  fmap f c = Cont $ \g -> (f . runCont c) g
instance Applicative (Cont r) where
  pure a = Cont ($ a)
  
  
instance 
instance Monad (Cont r) where
  return a = Cont ($ a)
  m >>= k  = Cont $ \c -> runCont m $ \a -> runCont (k a) c
