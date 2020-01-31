module MP.MP1 where

-- Part 1: Type declarations (please don't cheat with `:t`!)

p1_ex1 a b f g = h a
  where h x = f (g x b)

p1_ex2 = undefined

p1_ex3 = undefined

p1_ex4 = undefined

-- Part 2: Polymorphic functions from types

p2_ex1 :: a -> b -> b
p2_ex1 = undefined

p2_ex2 :: (a -> b -> c) -> (a,b) -> c
p2_ex2 = undefined

p2_ex3 :: (a -> b) -> (b -> c) -> a -> c
p2_ex3 = undefined

p2_ex4 :: (a -> b -> c) -> a -> (d -> b) -> d -> c
p2_ex4 = undefined

-- Part 3: Function implementations 

p3_ex1 :: a
p3_ex1 = undefined

p3_ex2 :: a
p3_ex2 = undefined

p3_ex3 :: a
p3_ex3 = undefined

p3_ex4 :: a
p3_ex4 = undefined

-- Part 4: Writing tests: update MP1Spec.hs for 100% coverage!
