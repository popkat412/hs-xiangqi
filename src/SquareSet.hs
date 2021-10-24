{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SquareSet
  ( SquareSet (..),
    Square (..),
    File (..),
    Rank (..),
    Side (..),
    BoardDir (..),
    CompassDir (..),
    KnightDir (..),
    ElephantDir (..),
    PieceContext (..),
    getPieceContext,
    getBitAtDir,
    getFile,
    getRank,
    universal,
    SquareSet.empty,
    one,
    palaceMask,
    shiftSS,
    shiftSquare,
    forFile,
    notFile,
    forFiles,
    forRank,
    forRanks,
    isEmpty,
    isUniversal,
    intersection,
    isSubset,
    isDisjoint,
    union,
    complement,
    relativeComplement,
    isComplement,
    SquareSet.xor,
    prettySquareSet,
    prettyPieceContext,
    getBit,
    setBit,
    setBits,
    clearBit,
    toggleBit,
  )
where

import Data.Bits (Bits, FiniteBits, shiftL, shiftR, (.&.), (.|.))
import qualified Data.Bits as Bits
import Data.DoubleWord (Word96 (..))
import Data.List (foldl', intersperse)
import Data.Word (Word8)
import Helpers (safeToEnum, showBits, splitEvery)
import Test.QuickCheck.Arbitrary

type Bit = Bool

{- ORMOLU_DISABLE -}
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

data File = A | B | C | D | E | F | G | H | I
  deriving stock (Eq, Enum, Ord, Show)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  deriving stock (Eq, Enum, Ord, Show)
{- ORMOLU_ENABLE -}

data Side = Red | Black deriving stock (Eq, Show, Enum)

-- {{{ Squares, Files and Ranks

getFile :: Square -> File
getFile sq = toEnum $ fromEnum sq `mod` 9

getRank :: Square -> Rank
getRank sq = toEnum $ fromEnum sq `div` 9

-- }}}

-- {{{ Compass rose,

class (Enum a, Bounded a) => BoardDir a where
  offset :: a -> Int
  outOfBoundsMask :: a -> SquareSet

-- {{{ Compass Dir

-- | The 8 "normal" directions of the compass.
data CompassDir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
  deriving stock (Eq, Show, Enum, Bounded)

instance BoardDir CompassDir where
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

instance BoardDir KnightDir where
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
data ElephantDir = ElephantNE | ElephantSE | ElephantSW | ElephantNW
  deriving stock (Eq, Show, Enum, Bounded)

instance BoardDir ElephantDir where
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
shiftSS :: (BoardDir a) => a -> SquareSet -> SquareSet
shiftSS dir =
  let off = offset dir
      shiftFn = if off > 0 then shiftL else shiftR
   in (`shiftFn` abs off)

shiftSquare :: (BoardDir a) => Square -> a -> Maybe Square
shiftSquare sq dir =
  if getBit sq (outOfBoundsMask dir)
    then Nothing -- the target will be out of the board
    else safeToEnum $ fromEnum sq + offset dir

-- }}}

-- }}}

-- {{{ Piece Context (for blockable pieces)

-- | This represents the 8 surrounding squares of a piece for memoization of blockable piece moves
-- | (note that memoization has not been implemented yet).
-- | The bits represent the eight compass directions Dir as defined above.
-- | For example, the 1st bit from the right will be North, the 2nd bit NorthEast, ...
newtype PieceContext = PieceContext Word8 deriving newtype (Eq, Bits, FiniteBits)

instance Show PieceContext where
  show = showBits

{- ORMOLU_DISABLE -}
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
              then acc
              else acc .|. PieceContext 1 `shiftL` idx
        )
        (PieceContext 0)
        (zip [0 ..] xs)

getBitAtDir :: CompassDir -> PieceContext -> Bit
getBitAtDir dir = (/= PieceContext 0) . (.&. PieceContext 1 `shiftL` fromEnum dir)

-- }}}

-- {{{ SquareSet

-- | Bitboard using Little-Endian Rank-File mapping (https://www.chessprogramming.org/Square_Mapping_Considerations#Little-Endian_Rank-File_Mapping)
newtype SquareSet = SquareSet Word96 deriving newtype (Bits, FiniteBits)

-- {{{ Instances
instance Eq SquareSet where
  -- Ignore the most significant 6 bits because the board has 90 squares but it isn't a power of 2
  (SquareSet a) == (SquareSet b) = (a .&. unusedBitsMask) == (b .&. unusedBitsMask)
    where
      unusedBitsMask :: Word96
      unusedBitsMask = 0x3FFFFFFFFFFFFFFFFFFFFFF -- most significant 6 bits off, everything else on

instance Arbitrary SquareSet where
  arbitrary = SquareSet <$> (Word96 <$> arbitrary <*> arbitrary)

instance Show SquareSet where
  show = showBits

-- }}}

prettySquareSet :: SquareSet -> String
prettySquareSet =
  unlines
    . (++ ["", '\t' : intersperse ' ' ['A' .. 'I']])
    . zipWith (++) (map (\a -> show a ++ "\t") [10, 9 .. 1 :: Int])
    . map (intersperse ' ' . reverse)
    . splitEvery 9
    . drop 6
    . showBits

-- {{{ Constants

{-
  10	1 1 1 1 1 1 1 1 1
  9	  1 1 1 1 1 1 1 1 1
  8	  1 1 1 1 1 1 1 1 1
  7	  1 1 1 1 1 1 1 1 1
  6	  1 1 1 1 1 1 1 1 1
  5	  1 1 1 1 1 1 1 1 1
  4	  1 1 1 1 1 1 1 1 1
  3	  1 1 1 1 1 1 1 1 1
  2	  1 1 1 1 1 1 1 1 1
  1	  1 1 1 1 1 1 1 1 1

      A B C D E F G H I
-}
universal :: SquareSet
universal = SquareSet 0xFFFFFFFFFFFFFFFFFFFFFFFF -- 2^96 - 1

{-
  10	0 0 0 0 0 0 0 0 0
  9	  0 0 0 0 0 0 0 0 0
  8	  0 0 0 0 0 0 0 0 0
  7	  0 0 0 0 0 0 0 0 0
  6	  0 0 0 0 0 0 0 0 0
  5	  0 0 0 0 0 0 0 0 0
  4	  0 0 0 0 0 0 0 0 0
  3	  0 0 0 0 0 0 0 0 0
  2	  0 0 0 0 0 0 0 0 0
  1	  0 0 0 0 0 0 0 0 0

      A B C D E F G H I
-}
empty :: SquareSet
empty = SquareSet 0

{-
  10	0 0 0 0 0 0 0 0 0
  9	  0 0 0 0 0 0 0 0 0
  8	  0 0 0 0 0 0 0 0 0
  7	  0 0 0 0 0 0 0 0 0
  6	  0 0 0 0 0 0 0 0 0
  5	  0 0 0 0 0 0 0 0 0
  4	  0 0 0 0 0 0 0 0 0
  3	  0 0 0 0 0 0 0 0 0
  2	  0 0 0 0 0 0 0 0 0
  1	  1 0 0 0 0 0 0 0 0

      A B C D E F G H I
-}
one :: SquareSet
one = SquareSet 1

-- TODO: Optimise
--- {{{ Files
forFile :: File -> SquareSet
forFile file =
  let fileNum = fromEnum file
   in foldl' (\bb num -> setBit (toEnum num) bb) empty [fileNum, fileNum + 9 .. 89]

notFile :: File -> SquareSet
notFile = complement . forFile

forFiles :: [File] -> SquareSet
forFiles = foldl' union empty . map forFile

-- }}}

-- {{{ Ranks
forRank :: Rank -> SquareSet
forRank rank =
  let rankNum = fromEnum rank
      startIdx = rankNum * 9
   in foldl' (\bb num -> setBit (toEnum num) bb) empty [startIdx .. startIdx + 8]

forRanks :: [Rank] -> SquareSet
forRanks = foldl' union empty . map forRank

-- }}}

-- {{{ Palace
{-# INLINE palaceMask #-}
palaceMask :: Side -> SquareSet
palaceMask Red = SquareSet 0xE07038
palaceMask Black = SquareSet 0x70381C0000000000000000

-- }}}

-- }}}

-- {{{ Predicates
isEmpty :: SquareSet -> Bool
isEmpty = (== SquareSet.empty)

isUniversal :: SquareSet -> Bool
isUniversal = (== universal)

intersection :: SquareSet -> SquareSet -> SquareSet
intersection = (.&.)

isSubset :: SquareSet -> SquareSet -> Bool
a `isSubset` b = (a `intersection` b) == a

isDisjoint :: SquareSet -> SquareSet -> Bool
a `isDisjoint` b = isEmpty $ a `intersection` b

-- }}}

-- {{{ Set operations
union :: SquareSet -> SquareSet -> SquareSet
union = (.|.)

complement :: SquareSet -> SquareSet
complement = Bits.complement

relativeComplement :: SquareSet -> SquareSet -> SquareSet
relativeComplement a = Bits.complement . (a .&.)

isComplement :: SquareSet -> SquareSet -> Bool
a `isComplement` b = complement a == b

xor :: SquareSet -> SquareSet -> SquareSet
xor = Bits.xor

-- }}}

-- {{{ Helpers

squareSetToBit :: SquareSet -> Bit
squareSetToBit = (/= empty)

squareToSquareSet :: Square -> SquareSet
squareToSquareSet sq = one `shiftL` fromEnum sq

-- }}}

-- {{{ Get bit / Set bit

getBit :: Square -> SquareSet -> Bit
getBit sq ss = squareSetToBit $ ss .&. squareToSquareSet sq

setBit :: Square -> SquareSet -> SquareSet
setBit sq = (.|. one `shiftL` fromEnum sq)

setBits :: SquareSet -> [Square] -> SquareSet
setBits = foldl' (flip setBit)

clearBit :: Square -> SquareSet -> SquareSet
clearBit sq = (.&. (complement $ one `shiftL` fromEnum sq))

toggleBit :: Square -> SquareSet -> SquareSet
toggleBit sq ss = if getBit sq ss then setBit sq ss else clearBit sq ss

-- }}}

-- }}}
