% CS 340: Programming Paradigms and Patterns
% Lect 24 - Monadic Parsing
% Michael Lee

> module Lect.Lect24 where
> import Data.Char

Monadic Parsing
===============

State Monad
-----------

> data State s a = State { run :: s -> Maybe (a, s) }
>
> instance Functor (State s) where
>   fmap f st = State $ \s -> case run st s of
>                               Nothing -> Nothing
>                               Just (x, s') -> Just (f x, s')
>
> instance Applicative (State s) where
>   pure x = State $ \s -> Just (x, s)
>
>   stf <*> stx = State $ \s -> case run stf s of
>                                 Nothing -> Nothing
>                                 Just (f, s') -> run (fmap f stx) s'
>
> instance Monad (State s) where
>   st >>= f = State $ \s -> case run st s of
>                              Nothing -> Nothing
>                              Just (x, s') -> run (f x) s'
>
> type Parser a = State String a
>
> item :: Parser Char
> item = State $ \inp -> case inp of "" -> Nothing
>                                    (x:xs) -> Just (x, xs)
>

> class Applicative f => Alternative f where
>   empty :: f a
>   (<|>) :: f a -> f a -> f a
> 
>   many :: f a -> f [a]
>   some :: f a -> f [a]
> 
>   many x = some x <|> pure []
>   some x = pure (:) <*> x <*> many x
> 
> instance Alternative (State s) where
>   empty = State $ \s -> Nothing
>   p <|> q = State $ \s -> case run p s of
>                             Nothing -> run q s
>                             r -> r
  
> sat :: (Char -> Bool) -> Parser Char
> sat p = do x <- item
>            if p x then return x else empty
>
> char :: Char -> Parser Char
> char c = sat (==c)
>
> string :: String -> Parser String
> string "" = return ""
> string (x:xs) = do char x
>                    string xs
>                    return (x:xs)
>
> digit :: Parser Char
> digit = sat isDigit
>
> space :: Parser ()
> space = do many (sat isSpace)
>            return ()
>
> nat :: Parser Int
> nat = do xs <- some digit
>          return (read xs)
>
> int :: Parser Int
> int = (do char '-'
>           n <- nat
>           return (-n))
>       <|> nat
>
> token :: Parser a -> Parser a
> token p = do space
>              x <- p
>              space
>              return x
>
> symbol :: String -> Parser String
> symbol s = token (string s)
>
> integer :: Parser Int
> integer = token int
>
> ints :: Parser [Int]
> ints = do symbol "["
>           n <- integer
>           ns <- many (do symbol ","
>                          integer)
>           symbol "]"
>           return (n:ns)
> 





