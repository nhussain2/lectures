% CS 340: Programming Paradigms and Patterns
% Lect 10 - Recursive types
% Michael Lee

> module Lect.Lect10 where

Recursive types
===============

Lists
-----

> data List a = Cons a (List a) | Empty deriving Show
>
> head' :: List a -> a
> head' Empty = error "Empty list"
> head' (Cons x _) = x
>
> tail' :: List a -> List a
> tail' Empty = error "Empty list"
> tail' (Cons _ xs) = xs
>
> takel :: Int -> List a -> List a
> takel 0 _ = Empty
> takel n (Cons x xs) = Cons x $ takel (n-1) xs
>
> repeatl :: a -> List a
> repeatl x = Cons x $ repeatl x
>
> mapl :: (a -> b) -> List a -> List b
> mapl _ Empty = Empty
> mapl f (Cons x xs) = Cons (f x) (mapl f xs)
>
> foldrl :: (a -> b -> b) -> b -> List a -> b
> foldrl f z Empty = z
> foldrl f z (Cons x xs) = f x $ foldrl f z xs
>
> suml :: Num a => List a -> a
> suml = foldrl (+) 0 


Binary Trees
------------

> data BinTree a = Node (BinTree a) a (BinTree a) | Leaf a
>   deriving Show
>
> eTree :: BinTree Char
> eTree = Node (Node (Leaf '3') '+' (Leaf '5')) '*' (Leaf '9')
>
> treeElem :: Eq a => a -> BinTree a -> Bool
> treeElem x (Leaf y) = x == y
> treeElem x (Node l y r) = x == y || treeElem x l || treeElem x r
>
> flatten :: BinTree a -> [a]
> flatten (Leaf x) = [x]
> flatten (Node l x r) = flatten l ++ [x] ++ flatten r
>
> infTree :: BinTree Integer
> infTree = t 1
>   where t n = Node (t (n*2)) n (t (n*2+1))
>
> prune :: Int -> BinTree a -> BinTree a
> prune 1 (Node _ x _) = Leaf x
> prune n (Node l x r) = Node (prune (n-1) l) x (prune (n-1) r)
>
> mapT :: (a -> b) -> BinTree a -> BinTree b
> mapT f (Leaf x) = Leaf $ f x
> mapT f (Node l x r) = Node (mapT f l) (f x) (mapT f r)
>
> foldrT :: (a -> b -> b) -> b -> BinTree a -> b
> foldrT f b (Leaf x) = f x b
> foldrT f b (Node l x r) = let b' = foldrT f b r
>                               b'' = f x b'
>                           in foldrT f b'' l


N-way Trees
-----------

> data Tree a = T a [Tree a] deriving Show
>
> infTree' :: Tree Integer
> infTree' = t 1
>   where t n = T n [t (n*2), t (n*2+1)]
>
> prune' :: Int -> Tree a -> Tree a
> prune' 1 (T x _) = T x []
> prune' n (T x ts) = T x $ map (prune' (n-1)) ts

