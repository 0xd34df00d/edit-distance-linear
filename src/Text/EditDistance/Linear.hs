{-# LANGUAGE BangPatterns, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.EditDistance.Linear where

import qualified Data.Array.Base as A(unsafeRead, unsafeWrite)
import qualified Data.Array.ST as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import Control.Monad.ST
import Data.Bits
import Data.Word

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
      let go !j !prev | j == n = pure ()
                      | otherwise = do
            delCost <- v0 `A.unsafeRead` (j + 1)
            substCostBase <- v0 `A.unsafeRead` j
            let !substCost = if s1char == s2 `BU.unsafeIndex` j then 0 else 1
            let !res = min (substCost + substCostBase) $ 1 + min delCost prev
            A.unsafeWrite v1 (j + 1) res
            go (j + 1) res
      go 0 (i + 1)
      loop (i + 1) v1 v0

newtype MatchMatrix s = MatchMatrix { getMatchMatrix :: A.STArray s Word8 (Maybe (A.STUArray s Int Word64)) }

matchVector :: MatchMatrix s -> Word8 -> ST s (Maybe (A.STUArray s Int Word64))
matchVector (MatchMatrix m) w = do
  bounds <- A.getBounds m
  if A.inRange bounds w
    then A.readArray m w
    else pure Nothing

matchMatrix :: BS.ByteString -> ST s (MatchMatrix s)
matchMatrix str = MatchMatrix <$> do
  mat <- A.newArray (minChar, maxChar) Nothing
  let go _ [] = pure mat
      go !idx ((!c):cs) = do
        vec <- A.readArray mat c >>=
                \case Just vec -> pure vec
                      Nothing -> do
                        newVec <- A.newArray (0, 1 + BS.length str `div` 8) 0
                        A.writeArray mat c $ Just newVec
                        pure newVec
        let (!wordPos, !bitPos) = idx `divMod` 8
        cur <- A.unsafeRead vec wordPos
        A.unsafeWrite vec wordPos $ cur .|. bit bitPos
        go (idx + 1) cs
  go 0 $ BS.unpack str
  where
    minChar = BS.minimum str
    maxChar = BS.maximum str
