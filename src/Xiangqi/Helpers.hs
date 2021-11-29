module Xiangqi.Helpers
  ( showBits,
    splitEvery,
    safeToEnum,
    padRight,
    count,
    genericBoardPrettyPrinter,
  )
where

import Data.Bits (FiniteBits, finiteBitSize, shiftR, testBit)
import Data.List (intersperse)

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

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

-- | This is an __internal and exception throwing__ generic pretty printer used in 'perttySquareSet'
-- and 'prettyBoard'. Use at your own risk.
genericBoardPrettyPrinter ::
  -- | Footer
  String ->
  -- | A [Char] of length 90, the data to be printed
  String ->
  -- | Pretty printed
  String
genericBoardPrettyPrinter footer =
  unlines -- wow such point free
    . (++ ["", replicate 5 ' ' ++ intersperse ' ' ['A' .. 'I'], footer])
    . zipWith (++) (map (padRight 5 ' ' . show) [10, 9 .. 1 :: Int])
    . map (intersperse ' ' . reverse)
    . splitEvery 9
