{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Criterion.Main
import Data.Text.Metrics

import Text.EditDistance.Linear

main :: IO ()
main = defaultMain
  [ bgroup "same string/ed-linear"      [ bench (show num) $ nf (\s -> levenshteinDistance s s) $ BS.replicate num 'a'
                                        | num <- nums
                                        ]
  , bgroup "same string/text-metrics"   [ bench (show num) $ nf (uncurry levenshtein) (T.replicate num "a", T.replicate num "a")
                                        | num <- nums
                                        ]
  , bgroup "diff strings/ed-linear"     [ bench (show num) $ nf (uncurry levenshteinDistance) (BS.replicate num 'a', BS.replicate num 'b')
                                        | num <- nums
                                        ]
  , bgroup "diff strings/text-metrics"  [ bench (show num) $ nf (uncurry levenshtein) (T.replicate num "a", T.replicate num "b")
                                        | num <- nums
                                        ]
  ]
  where
    nums = [ 1000, 2000, 5000, 10000, 20000 ]
