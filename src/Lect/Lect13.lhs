% CS 340: Programming Paradigms and Patterns
% Lect 13 - Functors, Applicatives, and Monads
% Michael Lee

> module Lect.Lect13 where
> import Prelude hiding (Functor, fmap, (<$>),
>                        Applicative, pure, (<*>), sequenceA,
>                        Monad, (>>=), (>>), return, fail,
>                        State)
> import qualified Prelude as P
> import Data.List

Functors, Applicatives, and Monads
==================================

Functors
--------

Functors are a class of data types that support a "mapping" operation.

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b

We can make a list a Functor, where fmap is identical to map

> instance Functor [] where
>   fmap = map

We can now do

> _ = fmap (*2) [1..10]

to map the function (*2) over the values in the list.

Because "fmap" is declared in a typeclass, we can define it separately for other
types, too (note we can't do this for "map", as it's a regular function with a
single, unique definition).

Here's the definition of fmap for the Maybe type:

> instance Functor Maybe where
>   fmap f Nothing = Nothing
>   fmap f (Just x) = Just (f x)

I.e., if we have a "Just" Maybe value, we reach inside the Just and apply the
fmap'd function to the value. If we have a "Nothing" Maybe value, there's no
contained value, so we just return Nothing.

Next, we start with definitions for a binary tree type ...

> data Tree a = Node (Tree a) a (Tree a) | Leaf a
>
> instance (Show a) => Show (Tree a) where
>   show t = s t 0
>     where s (Leaf x) n = replicate n '.' ++ show x ++ "\n"
>           s (Node l x r) n = replicate n '.'
>                                ++ show x ++ "\n"
>                                ++ s l (n+1) ++ s r (n+1)
> 
> treeDepth :: Int -> Tree Int
> treeDepth d = t 1 d
>   where t n d | d == 1 = Leaf n
>               | otherwise = Node (t (2*n) (d-1)) n (t (2*n+1) (d-1))

And then define fmap for the binary tree:

> instance Functor Tree where
>   fmap f (Leaf x) = Leaf $ f x
>   fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

"Lifting" functions
-------------------

By doing "fmap g", we now have a new version of the function g that can be
applied to any type that is a Functor!

Consider:

> liftedDouble :: (Functor f, Num a) => f a -> f a
> liftedDouble = fmap (*2)

We can do:

< liftedDouble [1..10]
< liftedDouble $ Just 5
< liftedDouble $ treeDepth 3

When we do this, we say that we have "lift"-ed the function -- i.e., made it
more abstract/general -- in this case so that it can be applied to arbitrary
Functors of the function's original input type.

But "fmap" is limited, in that it can only take a function of a single
argument. What if we want to lift functions of two or more arguments, so that we
can easily apply them to multiple functors containing those arguments?

We could achieve this with the following versions of fmap:

> fmap2 :: (a -> b -> c) -> f a -> f b -> f c
> fmap2 = undefined
> 
> fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
> fmap3 = undefined

Etc.


Applicative Functors
====================

The Applicative typeclass extends Functors with additional methods. "pure" takes
a value and wraps it in a Functor instance, while "<*>" applies a function found
in one functor to a value in another functor. The "<$>" operator is just an
infix version of fmap (we'll see how that's useful later).

> class (Functor f) => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b
>   (<$>) :: (a -> b) -> f a -> f b
>   (<$>) = fmap

Here's how the Maybe type is defined as an Applicative Functor:

> instance Applicative Maybe where
>   pure = Just
>   Nothing <*> _ = Nothing
>   Just f <*> Nothing = Nothing
>   Just f <*> Just x = Just $ f x

This time around, we can only carry out function application if both Maybe
values are Justs. If either is a Nothing, we are either lacking a function to
apply or a value to apply it to!

Now, we can do:

> _ = pure (*2) <*> Just 5

Here, the <*> "operator" is just like function application, except lifted to
functors.

More importantly, due to the currying of functions, we can also do:

> _ = pure (*) <*> Just 2 <*> Just 5

Or, equivalently:

> _ = fmap (*) (Just 2) <*> Just 5

Which allows us to simply write:

> _ = (*) <$> Just 2 <*> Just 5

If we wanted to, we could easily implement arbitrary arity lifts, e.g.,

> liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
> liftA2 f a1 a2 = f <$> a1 <*> a2
> 
> liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
> liftA3 f a1 a2 a3 = f <$> a1 <*> a2 <*> a3
>
> _ = let f = liftA3 (\x y z -> x + y - z)
>     in f (Just 1) (Just 3) (Just 5)

But this isn't necessary if we're willing to just write in the "applicative
style", i.e., using <*> for function application.


Functors as Computational Contexts
----------------------------------

We can think of Maybe qua Functor as providing a "computational context" for
values. "Just" represents a successful context, while "Nothing" represents a
failure. "pure" takes a value and gives us an initial (successful) context, and
the <*> operator allows us to combine it with other contexts, while
automatically detecting and propagating failure.

E.g., consider:

> _ = (\x y z -> x++y++z) <$> Just "Hello" <*> Just "Hola" <*> Just "Hi"
> _ = (\x y z -> x++y++z) <$> Just "Hello" <*> Just "Hola" <*> Nothing
> _ = (\x y z -> x++y++z) <$> Just "Hello" <*> Nothing     <*> Just "Hi"
> _ = (\x y z -> x++y++z) <$> Nothing      <*> Just "Hola" <*> Just "Hi"

The first expression is the only to succeed (with the result "HelloHolaHi"),
because all contexts being combined are "Just" values. In subsequent
expressions, the applicative machinery takes care of detecting the "Nothing" and
causing the entire expression to fail. Note that the original function (a
lambda, in this case) knows nothing about the possibility of failure.


Lists as Applicative Functors
-----------------------------

Here's how we make lists applicative functors.

> instance Applicative [] where
>   pure x = [x]
>   fs <*> xs = [f x | f <- fs, x <- xs]

The <*> operator for lists will go through all the different ways of combining
elements from its two arguments. When used as follows:

> _ = pure (+2) <*> [1..5] -- => [3,4,5,6,7]

I.e., where the first list only contains one value, this behaves just like fmap
(and map). When applied to multi-valued lists, however:

> _ = [(*2), (*3)] <*> [1..5] -- => [2,4,6,8,10,3,6,9,12,15]

or, equivalently:

> _ = pure (*) <*> [2,3] <*> [1..5]

We get the results of *all* the combinations of functions and arguments drawn
from the lists in the expression.

The computational context represented by a non-empty list can be thought of as
one where there are many different "answers" to the expression being
evaluated. The applicative machinery automatically combines list functors to
yield all these answers. This is sometimes referred to as *non-deterministic
programming* --- i.e., where we come up with not one, but all possible answers
to a particular problem.

Here's the cartesian product function implemented trivially with lifting:

> cartesianProd :: [a] -> [b] -> [(a,b)]
> cartesianProd = liftA2 (,)

But what happens when we insert an empty list into an applicative application?

> _ = pure (*) <*> [] <*> [1..10] -- => []

Just as with Nothing for the Maybe functor, empty lists represent "failure" ---
i.e., no possible way of computing a result.


The road forward
----------------

Applicatives are useful when we want to apply pure functions to functors, but
what happens when the functions we want to apply can themselves fail (i.e.,
produce some computational context)?

E.g., consider a collection of binary, numeric functions which each may either
fail or compute a result:

> binFuncA :: Num a => a -> a -> Maybe a
> binFuncA = undefined
>
> binFuncB :: Num a => a -> a -> Maybe a
> binFuncB = undefined
>
> binFuncC :: Num a => a -> a -> Maybe a
> binFuncC = undefined

Having used one of the functions, we'd like to use its result in another
function of the collection, and so on --- each time taking into account the
possibility of failure, e.g.,

    let r1 = binFuncA a b
        r2 = binFuncB r1 c
    in binFuncC r2 d

This code is incorrect, of course, because the results (r1, r2) of the functions
are Maybe values, while the inputs are just numbers.

The applicative style is also kind of awkward here:

    binFuncC <$> (binFuncB <$> (binFuncA <$> Just a <*> Just b) <*> Just c) Just d

So ... what we need is another type of functor that accept functions which
produce functors as output --- these outputs must somehow be combined with the
original functors in order to preserve all computational context.


Monads
------

The Monad typeclass further extends applicatives so that they support a new
operator, >>=, called "bind".

> class Applicative m => Monad m where
>   (>>=) :: m a -> (a -> m b) -> m b
>   return :: a -> m a
>   return = pure -- "default" implementation

The >>= operator takes a monad, applies a function to its contents to produces a
new monad, and combines (binds) the two monads to produce a final result. The
binding operation effectively combines the computational contexts of the
incoming monad and the one produced by the function.

> instance Monad Maybe where
>   Nothing >>= _ = Nothing
>   Just x >>= f = f x
>   return = Just

Let's consider some functions that produce Maybe monads, i.e., which return
values in a context of success or failure.

> censor :: String -> Maybe String
> censor "foobar" = Nothing
> censor s = Just s

> (/!) :: (Fractional a, Eq a) => a -> a -> Maybe a
> _ /! 0 = Nothing
> n /! m = Just $ n/m

Let's use them with the bind operator:

> _ = Just "okay" >>= censor -- => Just "okay"
> _ = Just "foobar" >>= censor -- => Nothing
> _ = Nothing >>= censor -- => Nothing

Note that the first two are equivalent to:

> _ = return "okay" >>= censor -- => Just "okay"
> _ = return "foobar" >>= censor -- => Nothing

A common pattern is to do:

> _ = Just "okay" >>= (\s -> censor s)

The lambda returns the result of "censor", which returns a monad, so it can be
used on the right side of the bind operator.

> _ = Just 20 >>= \x -> Just 10 >>= \y -> x /! y -- => 2.0

Keep in mind that lambdas (starting with '\') extend as far "to the right" as
they can. A parenthesized version clearly revealing the lambda structure is:

> _ = Just 20 >>= \x -> (Just 10 >>= \y -> (x /! y))

We can now chain together multiple (/!) expressions:

> _ = Just 10 >>= \w ->
>     Just 20 >>= \x ->
>     Just 40 >>= \y ->
>     y /! x >>= \z ->
>     w /! z -- => Just 5.0

Which can fail:

> _ = Just 10 >>= \w ->
>     Just 0  >>= \x ->
>     Just 40 >>= \y ->
>     y /! x >>= \z ->
>     w /! z -- => Nothing

This pattern is so common and useful that Haskell provides a special syntax:
"do" notation. The previous examples can be written:

> _ = do
>   s <- Just "okay"
>   censor s
>
> _ = do
>   w <- Just 10
>   x <- Just 20
>   y <- Just 40
>   z <- y /! x
>   w /! z

Each line in a do block represents a monadic value, and "<-" appears to allow us
to "extract" the contents of a monadic value. Behind the scenes, what's really
going on is that the bind operator (>>=) is automatically being invoked
"between" lines!


The List Monad
--------------

Just as with the list applicative, the list monad supports nondeterministic
programming via its bind operator.

> instance Monad [] where
>   xs >>= f = concat [f x | x <- xs]

Consider:

> noun_phrases = do
>   article <- ["The", "A", "This"]
>   adjective <- ["red", "quick", "fuzzy"]
>   noun <- ["fox", "couch", "torpedo"]
>   return $ intercalate " " [article, adjective, noun]

