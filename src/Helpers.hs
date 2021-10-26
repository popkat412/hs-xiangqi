module Helpers
  ( showBits,
    splitEvery,
    safeToEnum,
    padRight,
  )
where

import Data.Bits (FiniteBits, finiteBitSize, shiftR, testBit)

showBits :: FiniteBits a => a -> String
showBits bits =
  showBits' (finiteBitSize bits - 1)
  where
    showBits' :: Int -> String
    showBits' s
      | s >= 0 =
        let b = if testBit (shiftR bits s) 0 then '1' else '0'
         in b : showBits' (pred s)
      | otherwise = ""

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where
    (as, bs) = splitAt n xs

safeToEnum :: (Enum a, Bounded a) => Int -> Maybe a
safeToEnum = enumIfBetween minBound maxBound
  where
    enumIfBetween :: (Enum a) => a -> a -> Int -> Maybe a
    enumIfBetween a z x =
      let a' = fromEnum a
          z' = fromEnum z
       in if a' <= x && x <= z' then Just $ toEnum x else Nothing

padRight :: Int -> a -> [a] -> [a]
padRight num x xs = take num $ xs ++ repeat x
