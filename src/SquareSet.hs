{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : SquareSet
-- Description : A square set/bitboard for Xiangqi
-- Copyright   : (c) Wang Yunze, 2021
-- License     : GPL-3
-- Stability   : experimental
module SquareSet
  ( -- * Square set

    -- ** Data types
    SquareSet (..),
    Square (..),
    File (..),
    Rank (..),
    Side (..),

    -- ** Predicates
    isEmpty,
    isUniversal,
    intersection,
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
    SquareSet.empty,
    one,
    palaceMask,

    -- ** Set operations
    union,
    complement,
    relativeComplement,
    SquareSet.xor,

    -- ** Bit manipulation
    getBit,
    setBit,
    setBits,
    clearBit,
    toggleBit,

    -- * Board offsets
    BoardOffset (..),
    CompassDir (..),
    KnightDir (..),
    ElephantDir (..),

    -- * Piece context
    PieceContext (..),
    getPieceContext,
    getBitAtDir,
    getFile,
    getRank,
    shiftSquare,
    shiftSS,

    -- * Misc
    prettySquareSet,
    prettyPieceContext,
  )
where

import Data.Bits (Bits (testBit), FiniteBits, shift, shiftL, (.&.), (.|.))
import qualified Data.Bits as Bits
import Data.DoubleWord (Word96 (..))
import Data.List (foldl', intersperse)
import Data.Word (Word8)
import Helpers (padRight, safeToEnum, showBits, splitEvery)
import Test.QuickCheck.Arbitrary

type Bit = Bool

{- ORMOLU_DISABLE -}

-- | This datatype makes it easy to work with squares on the chess board.
-- 'A1' is at the bottom left and 'I10' is at the top right of the board.
-- From this perspective, the 'Red' side will be at the bottom.
-- 
-- The enum instance corresponds with the bit number in the square set / bitboard.
-- For example, 'A1' has an enum value of 0 and thus will be represented by the first bit from the right in the bitboard.
-- 
-- The reason this isn't implemented as a combination of 'File's and 'Rank's
-- is so it's cleaner to write @A1@ instead of @Square A R1@
data Square =
  A1  | B1  | C1  | D1  | E1  | F1  | G1  | H1  | I1  |
  A2  | B2  | C2  | D2  | E2  | F2  | G2  | H2  | I2  |
  A3  | B3  | C3  | D3  | E3  | F3  | G3  | H3  | I3  |
  A4  | B4  | C4  | D4  | E4  | F4  | G4  | H4  | I4  |
  A5  | B5  | C5  | D5  | E5  | F5  | G5  | H5  | I5  |
  A6  | B6  | C6  | D6  | E6  | F6  | G6  | H6  | I6  |
  A7  | B7  | C7  | D7  | E7  | F7  | G7  | H7  | I7  |
  A8  | B8  | C8  | D8  | E8  | F8  | G8  | H8  | I8  |
  A9  | B9  | C9  | D9  | E9  | F9  | G9  | H9  | I9  |
  A10 | B10 | C10 | D10 | E10 | F10 | G10 | H10 | I10
    deriving stock (Eq, Enum, Ord, Show, Bounded)
{- ORMOLU_ENABLE -}

-- | Convenience for specifying a file.
-- The A file is at the left and I is the right, from Red's perspective
data File = A | B | C | D | E | F | G | H | I
  deriving stock (Eq, Enum, Ord, Show)

-- | Convenience for specifying a rank.
-- The 1st rank is at the bottom and 10th is at the tpo, from Red's perspective
data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  deriving stock (Eq, Enum, Ord, Show)

-- | The side of the piece/player.
data Side = Red | Black deriving stock (Eq, Show, Enum)

-- {{{ Squares, Files and Ranks

getFile :: Square -> File
getFile sq = toEnum $ fromEnum sq `mod` 9

getRank :: Square -> Rank
getRank sq = toEnum $ fromEnum sq `div` 9

-- }}}

-- {{{ Compass rose,

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

-- {{{ Compass Dir

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

-- }}}

-- {{{ Knight Dir

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

-- }}}

-- {{{ Elephant Dir

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

-- {{{ Functions

-- | Shift an entire square set by a offset.
shiftSS :: (BoardOffset a) => a -> SquareSet -> SquareSet
shiftSS dir =
  let off = offset dir
   in (`shift` abs off)

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

-- }}}

-- {{{ Piece Context (for blockable pieces)

-- | This represents the 8 surrounding squares of a piece for memoization of blockable piece moves
-- (note that memoization has not been implemented yet).
-- The bits represent the eight compass directions Dir as defined above.
-- For example, the 1st bit from the right will be North, the 2nd bit NorthEast, ...
newtype PieceContext = PieceContext Word8 deriving newtype (Eq, Bits, FiniteBits)

instance Show PieceContext where
  show = showBits

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

-- {{{ SquareSet

-- | A square set is also known as a bitboard, for more info see <https://www.chessprogramming.org/Bitboards>.
-- In this case we're using a 'Word96' from "Data.DoubleWord" to represent the board, since the Xiangqi board is 9x10 instead of 8x8,
-- it doesn't fit nicely into a 64 bit integer.
--
-- This is using Little-Endian Rank-File mapping (<https://www.chessprogramming.org/Square_Mapping_Considerations#Little-Endian_Rank-File_Mapping>)
newtype SquareSet = SquareSet Word96 deriving newtype (Bits, FiniteBits)

-- {{{ Instances
instance Eq SquareSet where
  -- Ignore the most significant 6 bits because the board has 90 squares but it isn't a power of 2
  (SquareSet a) == (SquareSet b) = (a .&. unusedBitsMask) == (b .&. unusedBitsMask)
    where
      unusedBitsMask :: Word96
      unusedBitsMask = 0x3FFFFFFFFFFFFFFFFFFFFFF -- most significant 6 bits off, everything else on

instance Arbitrary SquareSet where
  -- For testing with QuickCheck
  arbitrary = SquareSet <$> (Word96 <$> arbitrary <*> arbitrary)

instance Show SquareSet where
  -- Show in binary instead of base 10 for easier debugging
  show = showBits

-- }}}

-- | Prints the Square Set in a nice board manner to aid debugging
prettySquareSet :: SquareSet -> String
prettySquareSet =
  unlines -- wow such point free
    . (++ ["", replicate 5 ' ' ++ intersperse ' ' ['A' .. 'I']])
    . zipWith (++) (map (padRight 5 ' ' . show) [10, 9 .. 1 :: Int])
    . map (intersperse ' ' . reverse)
    . splitEvery 9
    . drop 6
    . showBits

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

--- {{{ Files
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

-- }}}

-- {{{ Ranks

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

-- {{{ Palace
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

-- }}}

-- }}}

-- {{{ Predicates

{-# INLINE isEmpty #-}
isEmpty :: SquareSet -> Bool
isEmpty = (== SquareSet.empty)

{-# INLINE isUniversal #-}
isUniversal :: SquareSet -> Bool
isUniversal = (== universal)

{-# INLINE intersection #-}
intersection :: SquareSet -> SquareSet -> SquareSet
intersection = (.&.)

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

-- {{{ Set operations
{-# INLINE union #-}
union :: SquareSet -> SquareSet -> SquareSet
union = (.|.)

{-# INLINE complement #-}
complement :: SquareSet -> SquareSet
complement = Bits.complement

{-# INLINE relativeComplement #-}
relativeComplement :: SquareSet -> SquareSet -> SquareSet
relativeComplement a = Bits.complement . (a .&.)

{-# INLINE xor #-}
xor :: SquareSet -> SquareSet -> SquareSet
xor = Bits.xor

-- }}}

-- {{{ Get bit / Set bit

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

-- }}}

-- }}}
