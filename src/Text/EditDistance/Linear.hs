{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.EditDistance.Linear where

import qualified Data.Array.Base as A(unsafeRead, unsafeWrite)
import qualified Data.Array.ST as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BU
import Control.Monad
import Control.Monad.ST

levenshteinDistance :: BS.ByteString -> BS.ByteString -> Int
levenshteinDistance s1 s2 = runST $ do
  v0Init <- A.newListArray (0, n) [0..]
  v1Init <- A.newArray_ (0, n)
  loop 0 v0Init v1Init
  A.unsafeRead (if even m then v0Init else v1Init) n

  where
    m = BS.length s1
    n = BS.length s2

    loop :: Int -> A.STUArray s Int Int -> A.STUArray s Int Int -> ST s ()
    loop !i !v0 !v1 | i == m = pure ()
                    | otherwise = do
      A.unsafeWrite v1 0 (i + 1)
      let !s1char = s1 `BU.unsafeIndex` i
      let go !j | j == n = pure ()
                | otherwise = do
            delCost <- v0 `A.unsafeRead` (j + 1)
            insCost <- v1 `A.unsafeRead` j
            substCostBase <- v0 `A.unsafeRead` j
            let !substCost = if s1char == s2 `BU.unsafeIndex` j then 0 else 1
            A.unsafeWrite v1 (j + 1) $ min (substCost + substCostBase) $ 1 + min delCost insCost
            go (j + 1)
      go 0
      loop (i + 1) v1 v0
