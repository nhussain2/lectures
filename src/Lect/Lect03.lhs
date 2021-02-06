% CS 340: Programming Paradigms and Patterns
% Lect 03 - Functions
% Michael Lee

> module Lect.Lect03 where

Functions
=========

Agenda:
  - Defining functions
    - Pattern matching
    - Guards
    - `where` clause
  - Some useful language constructs
    - `if-else` expressions
    - `case` expressions
    - `let-in` expressions


Defining Functions
------------------

A named, top-level function definition starts with its name, is followed by its
parameter list (separated by spaces), then `=`, then an expression which will 
be evaluated to determine its result.

By convention, we always include type declarations for functions.

Examples:

> -- Exercise: add type declarations to the following functions
>
> nand :: Bool -> Bool -> Bool
> nand a b = not (a && b)
-- nand is not AND
-- takes BOOL BOOL and returns BOOL
>
>

-- => Anything in front of => is a type constraint, making it a polymorphic function

-- function that takes 3 arguments, a b c -- 
-- why not Int a => a -> a -> a -> a?
-- Int is too specific, we can't do whole numbers and definitions, Ints are also constrained based on size
-- Num is more general
> discriminant :: Num a => a -> a -> a -> a
> discriminant a b c = b^2 - 4*a*c
> 
>
>
-- Convert celcius to fahrenheit
-- c2f :: Float -> Float (wrong, why?)
-- higher precision number? Fails

-- instead, we'd use Fractional (more general type class)
> c2f ::  Fractional a => a -> a
> c2f c = 9/5 + 32
> 
 

-- Pattern matching
-- Functions are entirely defined by their inputs

We can provide multiple alternative expressions to be evaluated when a function
is called, which are differentiated based on *patterns* that are matched against
the parameter values. Patterns are matched in order (top to bottom); only the
first pattern to match has its corresponding expression evaluated.

> not' :: Bool -> Bool
> not' True = False
> not' False = True
-- if input of this function evaluates to this value, then change to this value
-- just look for all possible inputs, since there are only 2 inputs for a Boolean

-- ' added to not' so that we define itself, not' is our own function, not on its own already exists


A catch-all pattern, where a variable is specified instead of a data value, can 
be used to match parameters not specified in earlier patterns.

> fib :: Integer -> Integer
-- 0 1 1 2 3 5 8 13 21
> fib 0 = 0 -- first value of Fib sequence
> fib 1 = 1 -- second value of Fib sequence
> fib 2 = 1
> fib 3 = 2
-- now we need a more general for the rest of the function, otherwise it will only be a partial/incomplete function
-- we could keep on defining each fib, but we need a catch all, to generalize the rest of the pattern
> fib n = fib (n-2) + fib (n-1) -- horribly inefficient
-- pattern matches work, such that we start from top and go down, first pattern match found will be returned
-- order is important
-- if we don't define the base cases, it will go on forever

We can also use the wildcard pattern `_` to match on one or more values we don't
care about.

> nand' :: Bool -> Bool -> Bool
> nand' True True = False -- pattern to match (only pattern/case that returns False)
> nand' _ _ = True -- catch all '_', wildcard pattern. _ is a variable, that by convention means, we are not going to use it for anything else. Whatever is passed in, ignore it, the result does not pass through it. 


Patterns can also be used to "deconstruct" values. E.g., for tuples:

-- returns the type of the first value
> fst' :: (a,b) -> a
-- fst' (x,y) = x -- the tuple itself (x,y) is a value constructor
-- we pattern match on the tuple, then deconstruct the tuple and find the first value
-- since we do not care about the y, we can put an underscore there
> fst' (x,_) = x
-- if fst'(_,y) = y, it'll fail - it returns the type of second thing (Type b, Not necessarily Type a)
>
>
> distance :: (Floating a, Eq a) => (a,a) -> (a,a) -> a
-- distance p1 p2 = sqrt ((fst p1 - fst p2)^2 + (snd p1 - snd p2)^2)
-- instead of this, let's just deconstruct shape of data in parameters
> distance (x1, y1) ( x2, y2) = sqrt ((x1 - x2)^2 + (y1-y2)^2)
-- whenever possible, prefer pattern matching to programatic extraction

-- find second of 5?
-- sndof5 :: (a,b,c,d,e) -> b
-- sndof5 = (_,x,_,_,_) = x

> distanceFromOrigin :: (Floating a, Eq a) => (a,a) -> a
> distanceFromOrigin (x,0) = abs x
> distanceFromOrigin (0,y) = abs y
> distanceFromOrigin (x,y) = sqrt (x^2 + y^2)
>
>
> mapTup :: (a -> b) -> (a,a) -> (b,b)
-- first parameter, function that takes a, returns b
-- second parameter, tuple of 2 as
-- returns tuple of 2 bs
-- so in order to get 2 bs from 2 as, you'd need to apply the function to the 2as first
> mapTup f (x,y) = (f x, f y) -- pattern match


-- Guards
Boolean expressions can be used to provide more granular *guards* for separate
equations in a function definition. The `otherwise` keyword (which is just 
`True` in disguise) can be used to provide a catch-all equation.

> fib' :: Integer -> Integer
> fib' n | n = 0    = 0
>        | n = 1    = 1
>        | otherwise = fib' (n-1) + fib' (n-2) -- catch all, for all other cases 
>
>
> c2h :: (Floating a, Ord a) => a -> String -- celcius to human, takes number, returns string
> c2h c | c2f c < 50   = "that's cold!"
>       | c2f c > 95 = "that's hot!"
>       | otherwise   = "tolerable" -- ranges of values we are dealing with now, no more pattern matching, guards now.
-- anything following the equal sign is a defintion, anything before it, a guard
-- guard = definition
-- order matters from top to bottom
>
>
> quadRoots :: (Floating a, Ord a) => a -> a -> a -> (a, a)
> quadRoots a b c 
>   | discriminant a b c >= 0 = ((-b + sqrt (discriminant a b c)) / (2*a),
>                                (-b - sqrt (discriminant a b c)) / (2*a))
>   | otherwise = error "No real roots"


-- `where` clause

A `where` clause lets us introduce new local bindings (vars or functions) in a 
given function definition (which may span multiple guards, but *not* separate 
top-level patterns). Note that we read the `|` symbol as "such that".

> quadRoots' :: (Floating a, Ord a) => a -> a -> a -> (a, a)
> quadRoots' a b c 
>     | d >= 0 = ((-b + sqrt_d) / (2*a), (-b - sqrt_d) / (2*a))
>     | otherwise = error "No real roots"
>   where disc a b c = b^2 - 4*a*c -- this a b c are local parameters of function (full-fledged function definition, no corresponding type def bc it is a local function)
>         d          = disc a b c
>         sqrt_d     = sqrt d
-- syntactic that are about top-level function definitions

Some useful language constructs
-------------------------------
-- expressions exist within functions

An important note about the following constructs: they are all used to create
*expressions* --- i.e., they evaluate to values (which must have a consistent,
static type regardless of the evaluation path). They are not statements!


-- `if-else` expressions

The classic conditional (but both paths must exist, and must also evaluate to 
the same type!)

> -- try:
> -- oneOrOther n = if n < 0 then True else "False"
-- oneOrOther n = if n < 0 then True else "False"
-- above if else statement has to stay consistent with types
>
>
> fib'' :: Integer -> Integer
> fib'' n = if n <= 1 
>           then 1 
>           else fib'' (n-1) + fib'' (n-2)
>
> c2h'' :: (Floating a, Ord a) => a -> String
> c2h'' c | f >=0 && f <= 100 = "tolerable"
>         | otherwise         = "too " ++ if f < 0 then "cold" else "hot"
>   where f = c2f c


-- `case` expressions

-- top level function guard and pattern matching replacement

`case` expressions allow us to perform pattern matching --- just as we can 
across top-level function definitions --- on an arbitrary expression. Patterns
can also be followed by guards!

> distanceFromOrigin' :: (Floating a, Eq a) => (a,a) -> a
> distanceFromOrigin' p = case p of (x,0) -> abs x
>                                   (0,y) -> abs y
>                                   (x,y) -> sqrt (x^2 + y^2)
>
>
> greet :: String -> String
> greet name = "Hello" ++ 
>              case name of "Michael" -> " and welcome!" -- pattern match on name, of "" will pattern match towards
-- this isn't a function def, it's a case def, hence -> instead of =
-- pattern matches first, then looks for guards
>                           "Tom"     -> " friend."
>                           "Harry"   -> " vague acquaintance."
>                           name | null name -> " nobody." -- now you can use guards, if the name is empty (null name)
>                                | otherwise -> " stranger."
-- additional guards 
>              ++ " How are you?" -- string concatenation
-- since all of case expression is an expression, you can tag it onto something else.


-- `let-in` expressions

Similar to a `where` clause, the `let` keyword allows us to create new 
bindings, but only usable within the expression following the `in` keyword.
The entire `let-in` construct is also an *expression* --- it evaluates to the
value of the expression following `in`.

> quadRoots'' :: (Floating a, Ord a) => a -> a -> a -> (a, a)
> quadRoots'' a b c = let disc a b c = b^2 + 4*a*c
>                         d          = disc a b c
>                         sqrt_d     = sqrt d
>                     in if d >= 0 
>                        then ((-b + sqrt_d) / (2*a), (-b - sqrt_d) / (2*a))
>                        else error "No real roots"
>
>
> dist2h :: (Floating a, Ord a, Show a) => (a,a) -> String
> dist2h p = "The distance is " ++
>            let (x,y) = p
>                d = sqrt (x^2 + y^2)
>            in if d > 100 then "too far" else show d
