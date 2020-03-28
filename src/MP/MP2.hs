module MP.MP2 where

import Data.List
import Data.List.Split
import Test.QuickCheck

-- Tree defs

data BinTree a = Node a (BinTree a) (BinTree a)
data Direction = L | R


treeRepeat :: a -> BinTree a
treeRepeat = undefined


treeNats :: BinTree Integer
treeNats = undefined


treeVal :: [Direction] -> BinTree a -> a
treeVal = undefined


treeToList :: BinTree a -> [a]
treeToList = undefined


treeFlip :: BinTree a -> BinTree a
treeFlip = undefined


treeFromList :: [a] -> BinTree a
treeFromList = undefined


treeIterate :: (a -> a) -> a -> BinTree a
treeIterate = undefined


instance Functor BinTree where
  fmap = undefined


instance Applicative BinTree where
  pure = undefined
  (<*>) = undefined


-- Poker defs

data Card = Undefined


deck :: [Card]
deck = undefined


data Hand = HighCard  | Pair | TwoPair | ThreeOfAKind | Straight
            | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
            deriving (Eq, Show, Ord)


hand :: [Card] -> Hand
hand = undefined


computeStats :: [[Card]] -> [(Int, Hand)]
computeStats = undefined


-- you shouldn't need to change the functions below!

genDeck :: Gen [Card]
genDeck = shuffle deck

genHand :: Gen [Card]
genHand = (take 5) <$> genDeck

genHands :: Int -> Gen [[Card]]
genHands n = (take n . chunksOf 5) <$> genDeck

test :: Int -> IO [(Int, Hand)]
test n = generate $  computeStats <$> (vectorOf n genHand)
