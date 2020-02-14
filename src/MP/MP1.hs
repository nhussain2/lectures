module MP.MP1 where

import Data.Char

-- Part 1: Type declarations

p1_ex1 a b c = let d = head a
               in case b of (n, True) -> c < n
                            otherwise -> d


p1_ex2 a b c d e = (func a c, e ++ (b:d))
  where func x y = if x < y then x/y else y/x


p1_ex3 a b f g = h a  
  where h x = f (g x b)


p1_ex4 p f g x y | p x = f yy  
                 | otherwise = g xx y
  where yy = (head y, tail y)               
        xx = [ n * (head y) | n <- x ]


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

p3_unzip :: [(a,b)] -> ([a], [b])
p3_unzip = undefined


p3_removeAll :: (Eq a) => [a] -> [a] -> [a]
p3_removeAll = undefined

        
p3_luhn :: [Int] ->  Bool
p3_luhn = undefined


p3_runLengthEncode :: String -> [(Int,Char)]
p3_runLengthEncode = undefined


p3_runLengthDecode :: [(Int,Char)] -> String
p3_runLengthDecode = undefined


p3_vigenere :: String -> String -> String
p3_vigenere = undefined


-- Part 4: Writing tests: update MP1Spec.hs for 100% coverage!
