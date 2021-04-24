-- CS 340: Programming Paradigms and Patterns
-- Lect 11 - Monadic Parsing
-- Michael Lee

module Lect.Lect11 where
import Data.Char

data State s a = State { run :: s -> Maybe (a, s) }
-- computation given a state, may or may not succeed, if it succeeds, it creates a tuple with value and state and if it fails, it throws away the entire state and give Nothing.



instance Functor (State s) where
  fmap f st = State $ \s -> case run st s of
                              Nothing -> Nothing
                              -- if Nothing, return Nothing
                              Just(x, s') -> Just(f x, s')
                              -- apply f to value and pass on the updated state

instance Applicative (State s) where
  pure x = State $ \s -> Just (x, s)
  stf <*> stx = State $ \s -> case run stf s of
                              Nothing -> Nothing
                              Just (f, s') -> run (f <$> stx) s

instance Monad (State s) where
  st >>= f = State $ \s -> case run st s
                           Nothing -> Nothing
                           Just (x,s') -> run (f x) s'
                        
type Parser = State String a
-- takes string as input and polymorphic type a

item :: Parser Char
-- item :: State String Char
-- item :: String -> Maybe (Char, String)
item = State $ \input -> case input of "" -> Nothing -- failed parse on an empty string
                                      (x:xs) -> Just (x,xs)
                                      -- otherwise first character

-- satisfy parser
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
          if p x then return x else State(\_ -> Nothing)
-- check if predicate is true and return x (pure x)
-- otherwise we run a function that returns Nothing

-- p for predicate, p is our function

-- parser that checks for Char
char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string "" = return ""
-- parse x first, hence char x
string (x:xs) = do char x 
                   string xs
                   return $ x:xs

-- by binding using do, the input string gets carried along

-- character parser that is invoked recursively as many times  as necessary

digit :: Parser Char
digit = sat isDigit

-- old implementation
digits :: Parser [Char]
digits = do d <- digit
            ds <- digits 
            return $ d:ds

pOr :: Parser a -> Parser a -> Parser
p `pOr` q = State $ \s -> case run p s of
                               Nothing -> run q s
                               r -> r

digits' :: Parser [Char]
digits' = do d <- digit 
             ds <- digits' `pOr` return []
             return $ d:ds

onePlus :: Parser a -> Parser [a]
onePlus p = do x <- p
               xs <- onePlus p `pOr` return []
               return $ x:xs

digits'' = onePlus digit

zeroPlus :: Parser a -> Parser [a]
zeroPlus p = onePlus p `pOr` return []

onePlus' :: Parser a -> Parser [a]
onePlus' p :: pure (:) <*> p <*> zeroPlus p

class Applicative f => Alternative f where
  empty :: f a 
  (<|>) :: f a -> f a -> f a

  many :: f a -> f [a]
  some :: f a -> f [a]

  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many p

  instance Alternative (State s) where
    empty = State $ \_ -> Nothing
    p <|>\ q = State $ \x -> case run p s of
                                  Nothing -> run q s
                                  r -> r

sat' p = do x <- item
        if p x then return x else empty

-- some utility function, takes monad of a, returns monad of list of a's
digits''' = some digit

nat :: Parser Int
nat = read <$> digits'''

int :: Parser Int
int = (do char '-'
         n <- nat
         return $ -n) -- either this parser succeeds otherwise
      <|> nat -- this is the alternative parser

-- write parser for spaces
-- isSpace can come in handy

space :: Parser () -- we don't care what it parses, it throws it away
space = do many $ sat isSpace
           return ()

token :: Parser a -> Parser a
token p = do space
             x <- p
             space 
             return x

-- example of this is : run (token int) " -12   abc "

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol s = token (string s)

intList = do symbol "["
             n <- integer
             ns <- many (do symbol ","
                      integer)
            symbol "]"


typeName :: Parser String
typname = symbol "int" <|> symbol "char"

identifier :: Parser String
identifier = token (do c <- sat isLower
                      cs <- many (sat isAlphaNum)
                      return $ c:cs)
             

paramList :: Parser [String]
paramList = do typeName
               param <- many( do symbol ","
                                 typeName
                                 identifier)
               return $ param:params