{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString.Char8 as BS
import qualified Text.EditDistance as TED
import Test.Hspec
import Test.QuickCheck

import Text.EditDistance.Linear

baseLevDist :: String -> String -> Int
baseLevDist = TED.levenshteinDistance TED.defaultEditCosts

main :: IO ()
main = hspec $ do
  describe "Obvious cases" $ do
    it "Edit distance to self is always 0" $
      property $ \(BS.pack -> s) -> levenshteinDistance s s `shouldBe` 0
    it "Edit distance to completely different string is strictly positive" $ do
      property $ \(BS.pack . getNonEmpty -> s) -> levenshteinDistance s (BS.map succ s) > 0
    it "Edit distance to completely different string is not greater than string's length" $ do
      property $ \(BS.pack -> s) -> levenshteinDistance s (BS.map succ s) <= BS.length s
  describe "Comparing to edit-distance" $
    it "Edit distances always match" $
      property $ \(getASCIIString -> s1) (getASCIIString -> s2) -> levenshteinDistance (BS.pack s1) (BS.pack s2) `shouldBe` baseLevDist s1 s2
