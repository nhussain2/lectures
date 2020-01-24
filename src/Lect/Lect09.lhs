% CS 340: Programming Paradigms and Patterns
% Lect 09 - Type and Typeclass declarations
% Michael Lee

> module Lect.Lect09 where
> import Data.Char

Type and Typeclass declarations
===============================

Type Synonyms
-------------

`type` defines *type synonyms*. A type synonym is strictly a compile time
construct, and is used for legibility/documentation. At run-time, the type
synonym is replaced by the actual type in all contexts.

> type Letter = Char
> type Words = [Letter]
>
> caesar :: Words -> Int -> Words
> caesar "" _ = ""
> caesar s 0  = s
> caesar (c:cs) n = encrypt c : caesar cs n
>   where encrypt c
>           | isLetter c = chr ((ord (toUpper c) + n - ord 'A') `rem` 26
>                               + ord 'A')
>           | otherwise  = c
>
> type Point2D = (Double, Double)
> 
> distance :: Point2D -> Double
> distance (x,y) = sqrt $ x^2 + y^2
>
> type Vector3D = (Double, Double, Double)
>
> dot :: Vector3D -> Vector3D -> Double
> dot (a1,b1,c1) (a2,b2,c2) = a1*a2 + b1*b2 + c1*c2
>
> type IntMatrix = [[Int]]
>
> sumAll :: IntMatrix -> Int
> sumAll = sum . map sum


`newtype` defines new types from existing types, giving us a *data constructor*
(aka *value constructor*) we can use to create new values of the new type. We
can also pattern match against the value constructor

> newtype Flags = ListOfBools [Bool]
> 
> or' :: Flags -> Bool
> or' (ListOfBools []) = False
> or' (ListOfBools (x:xs)) = x || or' (ListOfBools xs)

Note:
- `Flags` is the type name, and `ListOfBools` is the data constructor.
- the data constructor is just a function that, when called with the field type,
  returns the type associated with the constructor

Because types and functions are in separate namespaces, it is possible (and
typical) to have overlapping names for types and data constructors.

> newtype Point3D = Point3D (Double, Double, Double)
>
> distance3D :: Point3D -> Double
> distance3D (Point3D (x,y,z)) = sqrt $ x^2 + y^2 + z^2


A type defined using `newtype` is similar to a type synonym in that it is
always based on a single existibng type, but a type defined using `newtype` is
not seen by the compiler as being equivalent to the type it is based on!

E.g., the following `distance3D'` function *will not* accept a Point3D value!

> distance3D' :: (Double, Double, Double) -> Double
> distance3D' (x,y,z) = sqrt $ x^2 + y^2 + z^2


Algebraic Data Types
--------------------

The `data` keyword allows us to create new data types with one or more
data constructors, each specifying any number of constituent types.

> data YesOrNo = Yes | No deriving Show
>
> (|||) :: YesOrNo -> YesOrNo -> YesOrNo
> No ||| No = No
> _ ||| _   = Yes

(The "deriving" keyword specifies what typeclasses we want this type to adopt.)

< foldr (|||) No $ repeat Yes -- works fine on infinite lists!

> data Shape = Circle Double | Triangle Double Double | Rectangle Double Double
>
> area :: Shape -> Double
> area (Circle r) = pi * r^2
> area (Triangle h b) = (h*b)/2
> area (Rectangle l w) = l*w

We can write functions to act as "getters" for specific data constructors, via
pattern matching:

> circRadius :: Shape -> Double
> circRadius (Circle r) = r

We can also use "record" syntax to define attribute names and automatically
generate "getter" functions:

> data Shape' = Circle' { radius :: Double }
>             | Triangle' { height :: Double, base :: Double }
>             | Rectangle' { length' :: Double, width :: Double }
>   deriving Show

This is more commonly used when defining a complex record:

> data Student = Student {
>   firstName :: String,
>   lastName  :: String,
>   studentId :: Integer,
>   grades    :: [Char]
> } deriving Show

< Student { firstName = "John", lastName = "Doe", studentId = 1234567, grades = [] }

Record syntax also provides a shortcut for "updating" a record value:

< let s = Student { firstName = "John", lastName = "Doe", studentId = 1234567, grades = [] }
< in s { grades = 'A' : grades s }

We can also define *self-referential* types --- i.e., a type where one or more
data constructors reference the type being defined.

> data Box = Box Int Box | EmptyBox deriving Show

< let b1 = EmptyBox
< let b2 = Box 1 EmptyBox
< let b3 = Box 1 (Box 2 (Box 3 (Box 4 EmptyBox)))

> outerMost :: Box -> Int
> outerMost EmptyBox = error "Box is empty"
> outerMost (Box x _) = x
>
> nestedBox :: Box -> Box
> nestedBox EmptyBox = error "Box is empty"
> nestedBox (Box _ b) = b


Polymorphic Types
-----------------

A polymorphic type is defined in terms of one or more other types (denoted
by type variables).

A common polymorphic type is `Maybe`, defined as:

< data Maybe a = Just a | Nothing

In a type declaration that uses `Maybe`, we can supply a type in place of the
type variable `a` to "complete" the Maybe type. Think of `Maybe` as a type
constructor that takes a type and produces a data type (a.k.a. a "proper" type
-- i.e., a type that has actual values).

E.g., `Maybe Bool` is a data type that has values `Nothing`, `Just True`, and
`Just False`.

We use `Maybe` to create data types that can represent both the absence of
the "contained" type (`Nothing`) or an actual value (`Just Val`).

> quadRoots :: Double -> Double -> Double -> Maybe (Double,Double)
> quadRoots a b c = let d = b^2-4*a*c
>                       sd = sqrt d
>                   in if d < 0
>                      then Nothing
>                      else Just ((-b+sd)/(2*a), (-b-sd)/(2*a))
> 
> find :: (a -> Bool) -> [a] -> Maybe a
> find _ [] = Nothing
> find p (x:xs) | p x = Just x
>               | otherwise = find p xs

Another polymorphic type is `Either`, defined as:

< data Either a b = Left a | Right b

We often use `Either` to create data types where the `Left` constructor
contains error values, and the `Right` constructor contains correct values.

> find' :: (a -> Bool) -> [a] -> Either String a
> find' _ [] = Left "List was empty"
> find' p (x:xs) | p x = Right x
>                | null xs = Left "No element satisifying predicate was found"
>                | otherwise = find' p xs
>
> elem' :: Eq a => a -> [a] -> Bool
> elem' x l = case find' (==x) l of Right _ -> True
>                                   Left _ -> False

Note that the `Either` type constructor takes two types to create a data/proper
type, while `Maybe` takes one. We use the term "kind" to describe the type of a
type constructor.

- every data/proper type has kind "*" (which we just read as "type")
- a unary type constructor has kind "* -> *" (e.g., `Maybe`)
- a binary type constructor has kind "* -> * -> *" (e.g., `Either`)
- ...

It is also possible to define higher-order type constructors (i.e., that take
other type constructors), e.g.,

  (* -> *) -> * -> *

A type with the above kind is:

> data T a b = T (a b)

(The :kind command in GHCi can be used to reveal the kind of a specified type.)


Typeclasses
-----------

A type class defines a collection of functions to be found in conforming types.

Types that conform to a type class are called *instances* of the class, and the
functions defined by the class are called *methods*.

Consider the built-in class `Eq`:

< class Eq a where
<   (==), (/=) :: a -> a -> Bool
<   x == y = not (x /= y)
<   x /= y = not (x == y)

And the student type we defined before:

data Student = Student {
   firstName :: String,
   lastName  :: String,
   studentId :: Integer,
   grades    :: [Char]
} 

> instance Eq Student where
>   (Student _ _ id1 _) == (Student _ _ id2 _) = id1 == id2

 instance Show Student where
   show (Student f l _ _) = f ++ " " ++ l


