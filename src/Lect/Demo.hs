module Lect.Demo where

primes = sieve [2..]
  where sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

nontailLength :: [a] -> Integer
nontailLength [] = 0
nontailLength (_:xs) = 1 + nontailLength xs

tailLength :: [a] -> Integer
tailLength xs = len xs 0
  where len [] n = n
        len (_:xs) n = len xs $! (n+1)

nontailSum :: (Eq a, Num a) => a -> a
nontailSum 0 = 0
nontailSum n = n + nontailSum (n-1)

tailSum :: (Eq a, Num a) => a -> a 
tailSum n = sum n 0
  where sum 0 r = r
        sum n r = sum (n-1) (n+r)

-- try `length $ XXXConcat $ replicate 100000 [1..100]` on each below

listcompConcat :: [[a]] -> [a]
listcompConcat ls = [x | l <- ls, x <- l]

nontailConcat :: [[a]] -> [a]
nontailConcat [] = []
nontailConcat (x:xs) = x ++ nontailConcat xs

tailConcat :: [[a]] -> [a]
tailConcat xs = conc xs []
  where conc [] r = reverse r
        conc ([]:xs) r = conc xs r
        conc ((x:xs):ys) r = conc (xs:ys) (x : r)

-- try `take 5 $ fst $ XXXPartition 100 [1..]` on each below

nontailPartition :: Ord a => a -> [a] -> ([a],[a])
nontailPartition n [] = ([],[])
nontailPartition n (x:xs) | x < n     = (x:lts, gts)
                          | otherwise = (lts, x:gts)
  where (lts, gts) = nontailPartition n xs

tailPartition :: Ord a => a -> [a] -> ([a],[a])
tailPartition n xs = part xs ([],[])
  where part [] r = r
        part (y:ys) (lts,gts) | y < n     = part ys (y:lts, gts)
                              | otherwise = part ys (lts, y:gts)

data Foo = Foo Int Int Int deriving Show

decon f@(Foo x y z) = (x,y,z,f)

permutations :: [a] -> [[a]]
permutations [] = []
permutations [x] = [[x]]
permutations [x,y] = [[x,y], [y,x]]
permutations (x:xs) = concat [ interleave x p | p <- permutations xs ]

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x [y] = [[x,y], [y,x]]
interleave x [y,z] = [[x,y,z], [y,x,z], [y,z,x]]
interleave x ys = [lhs ++ [x] ++ rhs 
                   | i <- [0..length ys], let (lhs, rhs) = splitAt i ys]

minmax :: Ord a => [a] -> (a,a)
minmax [x] = (x,x)
minmax (x:xs) = let (mn,mx) = minmax xs
                in (min x mn, max x mx)

elems :: Int -> [a] -> [a]
elems n xs = f 0 xs
  where 
    f _ [] = []
    f m (x:xs) | m `mod` n == 0 = x : f (m+1) xs
               | otherwise = f (m+1) xs

luhn xs = luhnSum False (reverse xs)
  where luhnSum _ [] = 0
        luhnSum even (x:xs) = (if even then luhnDouble x else x) 
                              + luhnSum (not even) xs
        luhnDouble n = let d = n*2 in if d > 9 then d-9 else d

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)        

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

fix f = let x = f x in x



data Student a = Student {
  name :: String,
  studId :: a,
  grades :: [Char]
} deriving Show

students :: [ Student Integer ]
students = [ 
  Student "Michael" 123456 ['A', 'B', 'C'],
  Student "Michael" 123456 ['A', 'B', 'C'],
  Student "Michael" 123456 ['A', 'B', 'C'],
  Student "Michael" 123456 ['A', 'B', 'C'],
  Student "Michael" 123456 ['A', 'B', 'C'],
  Student "Michael" 123456 ['A', 'B', 'C'],
  Student "Michael" 123456 ['A', 'B', 'C'] 
 ]

infixr 5 :-:
data List a = (:-:) a (List a) | Empty deriving Show

foo :: Num a => a -> a -> a -> a
foo x y z = x + y + z


infixr 0 $$
($$) :: (a -> b) -> a -> b
f $$ x = f x
