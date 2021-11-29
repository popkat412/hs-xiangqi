{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Xiangqi.SquareSet
-- Description : A square set/bitboard for Xiangqi
-- Copyright   : (c) Wang Yunze, 2021
-- License     : GPL-3
-- Stability   : experimental
module Xiangqi.SquareSet
  ( -- * Square set

    -- ** Predicates
    isEmpty,
    isUniversal,
    isSubset,
    isDisjoint,
    isComplement,

    -- ** Files and ranks
    forFile,
    notFile,
    forFiles,
    forRank,
    forRanks,

    -- ** Constants
    universal,
    Xiangqi.SquareSet.empty,
    one,
    palaceMask,
    redMask,
    blackMask,

    -- ** Set operations
    union,
    intersection,
    complement,
    relativeComplement,
    shiftSS,

    -- ** Square operations
    getFile,
    getRank,
    shiftSquare,

    -- ** Bit manipulation
    Xiangqi.SquareSet.xor,
    getBit,
    setBit,
    setBits,
    clearBit,
    toggleBit,
    bitScanForward,
    bitScanReverse,
    clearLS1B,
    clearMS1B,

    -- ** Misc
    squareSetToBool,
    traceSS,

    -- * Board offsets
    BoardOffset (..),
    CompassDir (..),
    KnightDir (..),
    ElephantDir (..),

    -- * Piece context
    PieceContext (..),
    getPieceContext,
    getBitAtDir,

    -- * Pretty printers
    prettySquareSet,
    prettyPieceContext,
  )
where

import Data.Bits (Bits (testBit), FiniteBits, countLeadingZeros, countTrailingZeros, finiteBitSize, shift, shiftL, (.&.), (.|.))
import qualified Data.Bits as Bits
import Data.List (foldl', intersperse)
import Data.Word (Word8)
import Debug.Trace (trace)
import Xiangqi.Helpers (genericBoardPrettyPrinter, safeToEnum, showBits)
import Xiangqi.Types

type Bit = Bool

-- {{{ Square Set

-- {{{ Predicates

{-# INLINE isEmpty #-}
isEmpty :: SquareSet -> Bool
isEmpty = (== Xiangqi.SquareSet.empty)

{-# INLINE isUniversal #-}
isUniversal :: SquareSet -> Bool
isUniversal = (== universal)

{-# INLINE isSubset #-}
isSubset :: SquareSet -> SquareSet -> Bool
a `isSubset` b = (a `intersection` b) == a

{-# INLINE isDisjoint #-}
isDisjoint :: SquareSet -> SquareSet -> Bool
a `isDisjoint` b = isEmpty $ a `intersection` b

{-# INLINE isComplement #-}
isComplement :: SquareSet -> SquareSet -> Bool
a `isComplement` b = complement a == b

-- }}}

-- {{{ Files and ranks

-- TODO: Memoize

-- | Similar to 'forRank', this gets the square set with all the bits for that file turned on.
--
-- __For example:__
--
-- >>> putStrLn $ prettySquareSet $ forFile A
-- 10   1 0 0 0 0 0 0 0 0
-- 9    1 0 0 0 0 0 0 0 0
-- 8    1 0 0 0 0 0 0 0 0
-- 7    1 0 0 0 0 0 0 0 0
-- 6    1 0 0 0 0 0 0 0 0
-- 5    1 0 0 0 0 0 0 0 0
-- 4    1 0 0 0 0 0 0 0 0
-- 3    1 0 0 0 0 0 0 0 0
-- 2    1 0 0 0 0 0 0 0 0
-- 1    1 0 0 0 0 0 0 0 0
-- <BLANKLINE>
--      A B C D E F G H I
forFile :: File -> SquareSet
forFile file =
  let fileNum = fromEnum file
   in foldl' (\bb num -> setBit (toEnum num) bb) empty [fileNum, fileNum + 9 .. 89]

-- | Convenience function for turning on every bit except the ones in the file.
--
-- Equivalent to: @complement . 'forFile'@
notFile :: File -> SquareSet
notFile = complement . forFile

-- | Similar to 'forRanks', this is a convenience function to get the union of multiple files.
--
-- /Equivalent to:/ @foldl\' 'union' 'empty' . map 'forFile'@
forFiles :: [File] -> SquareSet
forFiles = foldl' union empty . map forFile

-- | Similar to 'forFile', this returns the square set with all bits in the rank turned on.
--
-- __For example:__
--
-- >>> putStrLn $ prettySquareSet $ forRank R1
-- 10   0 0 0 0 0 0 0 0 0
-- 9    0 0 0 0 0 0 0 0 0
-- 8    0 0 0 0 0 0 0 0 0
-- 7    0 0 0 0 0 0 0 0 0
-- 6    0 0 0 0 0 0 0 0 0
-- 5    0 0 0 0 0 0 0 0 0
-- 4    0 0 0 0 0 0 0 0 0
-- 3    0 0 0 0 0 0 0 0 0
-- 2    0 0 0 0 0 0 0 0 0
-- 1    1 1 1 1 1 1 1 1 1
-- <BLANKLINE>
--     A B C D E F G H I
forRank :: Rank -> SquareSet
forRank rank =
  let rankNum = fromEnum rank
      startIdx = rankNum * 9
   in foldl' (\bb num -> setBit (toEnum num) bb) empty [startIdx .. startIdx + 8]

-- | Similar to 'forFiles', this is a convenience function to get the union of multiple ranks
--
-- /Equivalent to:/ @foldl' 'union' 'empty' . map 'forRank'@
forRanks :: [Rank] -> SquareSet
forRanks = foldl' union empty . map forRank

-- }}}

-- {{{ Constants

-- |
-- >>> putStrLn $ prettySquareSet $ universal
-- 10   1 1 1 1 1 1 1 1 1
-- 9    1 1 1 1 1 1 1 1 1
-- 8    1 1 1 1 1 1 1 1 1
-- 7    1 1 1 1 1 1 1 1 1
-- 6    1 1 1 1 1 1 1 1 1
-- 5    1 1 1 1 1 1 1 1 1
-- 4    1 1 1 1 1 1 1 1 1
-- 3    1 1 1 1 1 1 1 1 1
-- 2    1 1 1 1 1 1 1 1 1
-- 1    1 1 1 1 1 1 1 1 1
-- <BLANKLINE>
--      A B C D E F G H I
{-# INLINE universal #-}
universal :: SquareSet
universal = SquareSet 0xFFFFFFFFFFFFFFFFFFFFFFFF -- 2^96 - 1

-- |
-- >>> putStrLn $ prettySquareSet $ empty
-- 10   0 0 0 0 0 0 0 0 0
-- 9    0 0 0 0 0 0 0 0 0
-- 8    0 0 0 0 0 0 0 0 0
-- 7    0 0 0 0 0 0 0 0 0
-- 6    0 0 0 0 0 0 0 0 0
-- 5    0 0 0 0 0 0 0 0 0
-- 4    0 0 0 0 0 0 0 0 0
-- 3    0 0 0 0 0 0 0 0 0
-- 2    0 0 0 0 0 0 0 0 0
-- 1    0 0 0 0 0 0 0 0 0
-- <BLANKLINE>
--      A B C D E F G H I
{-# INLINE empty #-}
empty :: SquareSet
empty = SquareSet 0

-- |
-- >>> putStrLn $ prettySquareSet $ one
-- 10   0 0 0 0 0 0 0 0 0
-- 9    0 0 0 0 0 0 0 0 0
-- 8    0 0 0 0 0 0 0 0 0
-- 7    0 0 0 0 0 0 0 0 0
-- 6    0 0 0 0 0 0 0 0 0
-- 5    0 0 0 0 0 0 0 0 0
-- 4    0 0 0 0 0 0 0 0 0
-- 3    0 0 0 0 0 0 0 0 0
-- 2    0 0 0 0 0 0 0 0 0
-- 1    1 0 0 0 0 0 0 0 0
-- <BLANKLINE>
--      A B C D E F G H I
{-# INLINE one #-}
one :: SquareSet
one = SquareSet 1

{-# INLINE palaceMask #-}

-- | Masks the palace for a particular side.
--
-- __For example:__
--
-- >>> putStrLn $ prettySquareSet $ palaceMask Red
-- 10   0 0 0 0 0 0 0 0 0
-- 9    0 0 0 0 0 0 0 0 0
-- 8    0 0 0 0 0 0 0 0 0
-- 7    0 0 0 0 0 0 0 0 0
-- 6    0 0 0 0 0 0 0 0 0
-- 5    0 0 0 0 0 0 0 0 0
-- 4    0 0 0 0 0 0 0 0 0
-- 3    0 0 0 1 1 1 0 0 0
-- 2    0 0 0 1 1 1 0 0 0
-- 1    0 0 0 1 1 1 0 0 0
-- <BLANKLINE>
--      A B C D E F G H I
--
-- >>> putStrLn $ prettySquareSet $ palaceMask Black
-- 10   0 0 0 1 1 1 0 0 0
-- 9    0 0 0 1 1 1 0 0 0
-- 8    0 0 0 1 1 1 0 0 0
-- 7    0 0 0 0 0 0 0 0 0
-- 6    0 0 0 0 0 0 0 0 0
-- 5    0 0 0 0 0 0 0 0 0
-- 4    0 0 0 0 0 0 0 0 0
-- 3    0 0 0 0 0 0 0 0 0
-- 2    0 0 0 0 0 0 0 0 0
-- 1    0 0 0 0 0 0 0 0 0
-- <BLANKLINE>
--      A B C D E F G H I
palaceMask :: Side -> SquareSet
palaceMask Red = SquareSet 0xE07038
palaceMask Black = SquareSet 0x70381C0000000000000000

-- |
-- >>> putStrLn $ prettySquareSet $ redMask
-- 10   0 0 0 0 0 0 0 0 0
-- 9    0 0 0 0 0 0 0 0 0
-- 8    0 0 0 0 0 0 0 0 0
-- 7    0 0 0 0 0 0 0 0 0
-- 6    0 0 0 0 0 0 0 0 0
-- 5    1 1 1 1 1 1 1 1 1
-- 4    1 1 1 1 1 1 1 1 1
-- 3    1 1 1 1 1 1 1 1 1
-- 2    1 1 1 1 1 1 1 1 1
-- 1    1 1 1 1 1 1 1 1 1
-- <BLANKLINE>
--      A B C D E F G H I
redMask :: SquareSet
redMask = SquareSet 0x1fffffffffff

-- |
-- >>> putStrLn $ prettySquareSet $ blackMask
-- 10   1 1 1 1 1 1 1 1 1
-- 9    1 1 1 1 1 1 1 1 1
-- 8    1 1 1 1 1 1 1 1 1
-- 7    1 1 1 1 1 1 1 1 1
-- 6    1 1 1 1 1 1 1 1 1
-- 5    0 0 0 0 0 0 0 0 0
-- 4    0 0 0 0 0 0 0 0 0
-- 3    0 0 0 0 0 0 0 0 0
-- 2    0 0 0 0 0 0 0 0 0
-- 1    0 0 0 0 0 0 0 0 0
-- <BLANKLINE>
--      A B C D E F G H I
blackMask :: SquareSet
blackMask = SquareSet 0xffffffffffffe00000000000

-- }}}

-- {{{ Set operations

{-# INLINE union #-}
union :: SquareSet -> SquareSet -> SquareSet
union = (.|.)

{-# INLINE intersection #-}
intersection :: SquareSet -> SquareSet -> SquareSet
intersection = (.&.)

{-# INLINE complement #-}
complement :: SquareSet -> SquareSet
complement = Bits.complement

{-# INLINE relativeComplement #-}
relativeComplement :: SquareSet -> SquareSet -> SquareSet
relativeComplement a = Bits.complement . (a .&.)

-- | Shift an entire square set by a offset.
shiftSS :: (BoardOffset a) => a -> SquareSet -> SquareSet
shiftSS dir =
  (`shift` offset dir)

-- }}}

-- {{{ Square operations
getFile :: Square -> File
getFile sq = toEnum $ fromEnum sq `mod` 9

getRank :: Square -> Rank
getRank sq = toEnum $ fromEnum sq `div` 9

-- | Shift a square by a offset.
-- If the new square is out of bounds, it returns Nothing.
--
-- __For example:__
--
-- >>> shiftSquare A1 North
-- Just A2
shiftSquare :: (BoardOffset a) => Square -> a -> Maybe Square
shiftSquare sq dir =
  if getBit sq (outOfBoundsMask dir)
    then Nothing -- the target will be out of the board
    else safeToEnum $ fromEnum sq + offset dir

-- }}}

-- {{{ Bit manipulation
{-# INLINE xor #-}
xor :: SquareSet -> SquareSet -> SquareSet
xor = Bits.xor

{-# INLINE getBit #-}

-- | True is the bit at the square is set, else False.
getBit :: Square -> SquareSet -> Bit
getBit sq ss = testBit ss (fromEnum sq)

{-# INLINE setBit #-}

-- | Turn the bit at the square on.
setBit :: Square -> SquareSet -> SquareSet
setBit sq = (.|. one `shiftL` fromEnum sq)

{-# INLINE setBits #-}

-- | Convenience function for turning on multiple bits.
setBits :: SquareSet -> [Square] -> SquareSet
setBits = foldl' (flip setBit)

{-# INLINE clearBit #-}

-- | Turn the bit at the square off.
clearBit :: Square -> SquareSet -> SquareSet
clearBit sq = (.&. (complement $ one `shiftL` fromEnum sq))

{-# INLINE toggleBit #-}

-- | Toggle the bit at the square on / off.
toggleBit :: Square -> SquareSet -> SquareSet
toggleBit sq ss = if getBit sq ss then setBit sq ss else clearBit sq ss

bitScanForward :: SquareSet -> Maybe Square
bitScanForward = safeToEnum . countTrailingZeros

bitScanReverse :: SquareSet -> Maybe Square
bitScanReverse ss = safeToEnum $ finiteBitSize ss - countLeadingZeros ss - 1

clearLSorBS1B ::
  -- | bitScan function
  (SquareSet -> Maybe Square) ->
  -- | SquareSet to clear
  SquareSet ->
  -- | New square set
  Maybe SquareSet
clearLSorBS1B bitScan ss = do
  x <- bitScan ss
  return $ clearBit x ss

clearLS1B :: SquareSet -> Maybe SquareSet
clearLS1B = clearLSorBS1B bitScanForward

clearMS1B :: SquareSet -> Maybe SquareSet
clearMS1B = clearLSorBS1B bitScanReverse

-- }}}

-- {{{ Misc
squareSetToBool :: SquareSet -> Bool
squareSetToBool = (/= empty)

traceSS :: SquareSet -> a -> a
traceSS = trace . prettySquareSet

-- }}}

-- }}}

-- {{{ Board offsets

-- | A type which represents a offset from a Square, aka \"shifting\" the square somewhere
class (Enum a, Bounded a) => BoardOffset a where
  -- | This will be added to the current bit number to find the location of the new bit.
  --  For example, adding 1 essentially shifts the square left one space
  --  (unless it is already on the leftmost \"column\", in which case it will be shifted to the right of the row above)
  offset :: a -> Int

  -- | This returns a mask where if the square is inside the mask,
  --  then when shifted by the offset, will be out of the board.
  --  I.e. if @'getBit' currentSquare ('outOfBoundsMask' dir)@ is true, then @'shiftSquare' currentSquare dir@ will return @Nothing@.
  --
  --  Why not just check the result of 'shiftSquare'? Because internally, 'shiftSquare' uses this function.
  outOfBoundsMask :: a -> SquareSet

-- | The 8 "normal" directions of the compass.
-- This is used for pieces like Pawns, Kings and Advisors>
data CompassDir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
  deriving stock (Eq, Show, Enum, Bounded)

instance BoardOffset CompassDir where
  {-# INLINE offset #-}
  offset North = 9
  offset NorthEast = 10
  offset East = 1
  offset SouthEast = -8
  offset South = -9
  offset SouthWest = -10
  offset West = -1
  offset NorthWest = 8

  outOfBoundsMask North = forRank R10
  outOfBoundsMask NorthEast = forRank R10 `union` forFile I
  outOfBoundsMask East = forFile I
  outOfBoundsMask SouthEast = forFile I `union` forRank R1
  outOfBoundsMask South = forRank R1
  outOfBoundsMask SouthWest = forRank R1 `union` forFile A
  outOfBoundsMask West = forFile A
  outOfBoundsMask NorthWest = forFile A `union` forRank R10

-- | The 8 directions a knight can jump.
-- NNE stands for North North East, ENE stands for East North East, etc.
data KnightDir = KnightNNE | KnightENE | KnightESE | KnightSSE | KnightSSW | KnightWSW | KnightWNW | KnightNNW
  deriving stock (Eq, Show, Enum, Bounded)

instance BoardOffset KnightDir where
  {-# INLINE offset #-}
  offset KnightNNE = 19
  offset KnightENE = 11
  offset KnightESE = -7
  offset KnightSSE = -17
  offset KnightSSW = -19
  offset KnightWSW = -11
  offset KnightWNW = 7
  offset KnightNNW = 17

  outOfBoundsMask KnightNNE = forRanks [R10, R9] `union` forFile I
  outOfBoundsMask KnightENE = forRank R10 `union` forFiles [H, I]
  outOfBoundsMask KnightESE = forRank R1 `union` forFiles [H, I]
  outOfBoundsMask KnightSSE = forRanks [R1, R2] `union` forFile I
  outOfBoundsMask KnightSSW = forRanks [R1, R2] `union` forFile A
  outOfBoundsMask KnightWSW = forRank R1 `union` forFiles [A, B]
  outOfBoundsMask KnightWNW = forRank R10 `union` forFiles [A, B]
  outOfBoundsMask KnightNNW = forRanks [R10, R9] `union` forFile A

-- | The 4 directions an elephant can jump.
data ElephantDir = ElephantNE | ElephantSE | ElephantSW | ElephantNW
  deriving stock (Eq, Show, Enum, Bounded)

instance BoardOffset ElephantDir where
  {-# INLINE offset #-}
  offset ElephantNE = 20
  offset ElephantSE = -16
  offset ElephantSW = -20
  offset ElephantNW = 16

{- ORMOLU_DISABLE -}
  outOfBoundsMask ElephantNE = forRanks [R10, R9] `union` forFiles [H, I]
  outOfBoundsMask ElephantSE = forRanks [R1 , R2] `union` forFiles [H, I]
  outOfBoundsMask ElephantSW = forRanks [R1 , R2] `union` forFiles [A, B]
  outOfBoundsMask ElephantNW = forRanks [R10, R9] `union` forFiles [A, B]
{- ORMOLU_ENABLE -}

-- }}}

-- {{{ Piece context

-- | This represents the 8 surrounding squares of a piece for memoization of blockable piece moves
-- (note that memoization has not been implemented yet).
-- The bits represent the eight compass directions Dir as defined above.
-- For example, the 1st bit from the right will be North, the 2nd bit NorthEast, ...
newtype PieceContext = PieceContext Word8 deriving newtype (Eq, Bits, FiniteBits)

instance Show PieceContext where
  show = prettyPieceContext

-- | Gets the 'PieceContext' around a particular square.
-- If the square is out of bounds, then it is just set to zero.
--
-- __For example:__
--
-- >>> putStrLn $ prettyPieceContext $ getPieceContext E4 (empty & setBit E5)
-- 0 1 0
-- 0 X 0
-- 0 0 0
--
-- >>> putStrLn $ prettyPieceContext $ getPieceContext A1 universal
-- 0 1 1
-- 0 X 1
-- 0 0 0
getPieceContext :: Square -> SquareSet -> PieceContext
getPieceContext sq ss =
  toPieceContext $
    map
      ( \dir ->
          let target = sq `shiftSquare` dir
           in case target of
                Just x -> getBit x ss
                Nothing -> False
      )
      [(minBound :: CompassDir) ..]
  where
    toPieceContext :: [Bit] -> PieceContext
    toPieceContext xs =
      foldl'
        ( \acc (idx, x) ->
            if x
              then acc .|. PieceContext 1 `shiftL` idx
              else acc
        )
        (PieceContext 0)
        (zip [0 ..] xs)

-- | Helper function to get the bit at a particular 'CompassDir' of a 'PieceContext'.
--
-- __For example:__
--
-- >>> putStrLn $ prettyPieceContext $ PieceContext 1
-- 0 1 0
-- 0 X 0
-- 0 0 0
--
-- >>> getBitAtDir North (PieceContext 1)
-- True
--
-- >>> getBitAtDir South (PieceContext 1)
-- False
getBitAtDir :: CompassDir -> PieceContext -> Bit
getBitAtDir dir = (/= PieceContext 0) . (.&. PieceContext 1 `shiftL` fromEnum dir)

-- }}}

-- {{{ Pretty printers

-- | Prints the Square Set in a nice board manner to aid debugging
prettySquareSet :: SquareSet -> String
prettySquareSet ss@(SquareSet word) =
  genericBoardPrettyPrinter (show word) (drop 6 $ showBits ss)

{- ORMOLU_DISABLE -}
-- | Prints the 'PieceContext' in a nice board manner to aid debugging
prettyPieceContext :: PieceContext -> String
prettyPieceContext ctx =
  let rBinString = reverse $ showBits ctx
      f = (rBinString !!) . fromEnum
   in
        unlines $ map (intersperse ' ') [ -- this assumes that length binString == 8
          [f NorthWest, f North, f NorthEast],
          [f West     ,   'X'  , f East     ],
          [f SouthWest, f South, f SouthEast]
        ]
{- ORMOLU_ENABLE -}
