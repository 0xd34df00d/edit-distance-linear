module Main where

import qualified Data.ByteString.Char8 as BS
import Text.EditDistance.Linear

main :: IO ()
main = do
  let s1 = BS.replicate 10000 'a'
  let s2 = s1
  let s3 = BS.replicate 10000 'b'
  print $ levenshteinDistance s1 s2
  print $ levenshteinDistance s1 s3
