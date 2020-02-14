module MP.MP1Spec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Control.Exception
import Data.Char
import MP.MP1


spec :: Spec
spec = describe "MP1" $ do
  describe "p3_unzip" $ do
    it "works for the provided test cases" $ do
      p3_unzip [('a',1),('b',5),('c',8)] `shouldBe` ("abc",[1,5,8])
    it "produces lists of the correct length" $
      property prop_unzipLength
    it "works as the inverse of zip" $
      property prop_zipunzip

  describe "p3_removeAll" $ do
    it "works for the provided test cases" $ do
      p3_removeAll [1..3] [0..10] `shouldBe` [0,4,5,6,7,8,9,10]
      p3_removeAll "aeiou" "supercalifragilisticexpialidocious" `shouldBe` "sprclfrglstcxpldcs"

  describe "p3_luhn" $ do
    it "works for the provided test cases" $ do
      p3_luhn [2,7,5,8] `shouldBe` True
      p3_luhn [4,3,1,7,5,6,8] `shouldBe` False
      p3_luhn [3,9,2,8,6,4,1,7,2,0,5,2] `shouldBe` True
    it "has precisely one checksum value that works for a given value" $
      property prop_luhnOneSol

  describe "p3_runLengthEncode" $ do
    it "works for the provided test cases" $ do
      p3_runLengthEncode "aaaaaaabbb" `shouldBe` [(7,'a'),(3,'b')]
      p3_runLengthEncode "happy daaay" `shouldBe` [(1,'h'),(1,'a'),(2,'p'),(1,'y'),(1,' '),(1,'d'),(3,'a'),(1,'y')]
    it "doesn't encode adjacent duplicates" $
      property prop_runLengthEncodeNoAdjDups
    it "has only positive counts" $
      property prop_runLengthEncodePosCounts

  describe "p3_runLengthDecode" $ do
    it "works for the provided test cases" $ do
      p3_runLengthDecode [(1,'h'), (5,'i')] `shouldBe` "hiiiii"
      p3_runLengthDecode (p3_runLengthEncode "whhhhaaaaat?") `shouldBe` "whhhhaaaaat?"
    it "reverses run-length encoding" $
      property prop_runLengthCodec

  describe "p3_vigenere" $ do
    it "works for the provided test cases" $ do
      p3_vigenere "baz" "foobar" `shouldBe` "GONCAQ"
      p3_vigenere "Yadda" "Hello, world!" `shouldBe` "FEOOO, UOUOD!"


genString :: Gen String
genString = listOf $ elements $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']


prop_unzipLength :: [(Int,Int)] -> Bool
prop_unzipLength ts = let (l1,l2) = p3_unzip ts
                   in length l1 == length l2 && length l1 == length ts

                   
genVectorOfPair :: (Arbitrary a, Arbitrary b) => Gen ([a], [b])
genVectorOfPair = sized $ \n -> do
  l1 <- vectorOf n arbitrary
  l2 <- vectorOf n arbitrary
  return (l1, l2)


prop_zipunzip :: Property
prop_zipunzip = forAll genVectorOfPair $ \(l1, l2) -> 
    let l3 = zip l1 l2
        (l4,l5) = p3_unzip l3
        types = (l1 :: [Int], l2 :: [Int])
    in l1 == l4 && l2 == l5


genLuhnCandidates :: Gen [[Int]]
genLuhnCandidates = do 
  l <- listOf1 $ choose (0,9)
  return [l++[check] | check <- [0..9]]  


prop_luhnOneSol :: Property
prop_luhnOneSol = forAll genLuhnCandidates $ \cs ->
  length (filter p3_luhn cs) == 1


prop_runLengthCodec :: Property  
prop_runLengthCodec = forAll genString $ \s -> 
  p3_runLengthDecode (p3_runLengthEncode s) == s


prop_runLengthEncodeNoAdjDups :: Property
prop_runLengthEncodeNoAdjDups = forAll genString $ \s ->
    noAdjDups $ p3_runLengthEncode s
  where noAdjDups [] = True
        noAdjDups [x] = True
        noAdjDups s@((_,c1):(_,c2):cs) = c1 /= c2 && noAdjDups (tail s)


prop_runLengthEncodePosCounts :: Property
prop_runLengthEncodePosCounts = forAll genString $ \s ->
    all ((> 0) . fst) $ p3_runLengthEncode s
