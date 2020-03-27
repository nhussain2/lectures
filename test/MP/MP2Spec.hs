module MP.MP2Spec (spec) where

import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import MP.MP2

spec :: Spec
spec = describe "MP2" $
  it "works" $ do
    True `shouldBe` True
