module MP.MP3 where
import Data.Char 
import Data.List
import Control.Monad.Writer

newtype Parser a = Parser { parse :: String -> Maybe (String, a) }

pChar :: Parser Char
pChar = Parser $ \s -> case s of "" -> Nothing
                                 (c:cs) -> Just (cs, c)


-- Part 1: Functor, Applicative, Monad instances

instance Functor Parser where
  fmap = undefined

instance Applicative Parser where
  pure = undefined
  (<*>) = undefined

instance Monad Parser where
  (>>=) = undefined


-- Part 2: Basic parsers and utilities

pFail :: Parser a
pFail = Parser $ \_ -> Nothing


pIf :: (Char -> Bool) -> Parser Char
pIf pred = do c <- pChar
              if pred c 
              then return c 
              else pFail


pLetter :: Parser Char
pLetter = undefined


pDigit :: Parser Char
pDigit = undefined


pIs :: Char -> Parser Char
pIs = undefined


pEither :: Parser a -> Parser a -> Parser a
pEither = undefined


pOneOf :: [Parser a] -> Parser a
pOneOf = undefined


pSome :: Parser a -> Parser [a]
pSome = undefined


pAny :: Parser a -> Parser [a]
pAny = undefined


pSpace :: Parser ()
pSpace = undefined


pToken :: Parser a -> Parser a
pToken = undefined


pIdentifier :: Parser String
pIdentifier = undefined


-- Part 3: HTML parser

data DocTree = Element { elemName :: String, elemContent :: [DocTree] }
             | Attrib  { attribName :: String, attribVal :: String }
             | Text    { textContent :: String } 
             deriving (Show, Eq)


pHtml :: Parser DocTree
pHtml = undefined


execParser :: Parser a -> String -> a
execParser p s = let Just (_, v) = parse p s in v

-- Test inputs

htmlDoc1 = "<!DOCTYPE html><html></html>"

htmlDoc2 = "<!DOCTYPE html>\
           \<html>\
           \  <head></head>\
           \  <body>\
           \    <div>\
           \    </div>\
           \  </body>\
           \</html>"

htmlDoc3 = "<!DOCTYPE html>\
           \<html lang=\"en\">\
           \  <head>\
           \    <title>CS 340</title>\
           \  </head>\
           \  <body>\
           \    <div id=\"content\">\
           \      <p>Hello world!</p>\
           \    </div>\
           \  </body>\
           \</html>"

htmlDoc4 = "<!DOCTYPE html>\
           \<html lang=\"en\">\
           \  <head>\
           \    <title>CS 340</title>\
           \  </head>\
           \  <body>\
           \    <div id=\"content\" class=\"row\">\
           \      <p>This is a <strong>cool</strong> paragraph.</p>\
           \      <p>\
           \        This one is <span style=\"font-family: Helvetica;\">\
           \        not so cool.</span>\
           \      </p>\
           \    </div>\
           \    <div id=\"footer\">\
           \      <p>&copy; 2020</p>\
           \    </div>\
           \  </body>\
           \</html>"


-- Part 4: HTML generator

type HtmlWriter = Writer String ()


genHtml :: DocTree -> HtmlWriter
genHtml = undefined


htmlGen1 = execWriter $ genHtml $ execParser pHtml htmlDoc1 
htmlGen2 = execWriter $ genHtml $ execParser pHtml htmlDoc2
htmlGen3 = execWriter $ genHtml $ execParser pHtml htmlDoc3
htmlGen4 = execWriter $ genHtml $ execParser pHtml htmlDoc4
