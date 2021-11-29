{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Xiangqi.Types
-- Description : Shared data types among the library (to prevent cyclic deps)
-- Copyright   : (c) Wang Yunze, 2021
-- License     : GPL-3
-- Stability   : experimental
module Xiangqi.Types
  ( -- * SquareSet
    SquareSet (SquareSet),
    Square (..),
    File (..),
    Rank (..),

    -- * Board
    Board (..),

    -- ** Lenses
    getLens,
    occupied,
    red,
    black,
    rook,
    knight,
    elephant,
    advisor,
    king,
    pawn,
    cannon,

    -- * Role
    Role (..),

    -- * Side
    Side (..),

    -- * Piece
    Piece (..),

    -- ** Lenses
    side,
    role,
  )
where

import Data.Bits
import Data.DoubleWord (Word96 (Word96))
import Lens.Micro
import Lens.Micro.TH
import Test.QuickCheck (Arbitrary (arbitrary), arbitraryBoundedEnum)
import Xiangqi.Helpers (showBits)

-- {{{ Square set

-- | A square set is also known as a bitboard, for more info see <https://www.chessprogramming.org/Bitboards>.
-- In this case we're using a 'Word96' from "Data.DoubleWord" to represent the board, since the Xiangqi board is 9x10 instead of 8x8,
-- it doesn't fit nicely into a 64 bit integer.
--
-- This is using Little-Endian Rank-File mapping (<https://www.chessprogramming.org/Square_Mapping_Considerations#Little-Endian_Rank-File_Mapping>)
newtype SquareSet = SquareSet Word96 deriving newtype (Bits, FiniteBits)

instance Show SquareSet where
  show = showBits

-- show = prettySquareSet

instance Eq SquareSet where
  -- Ignore the most significant 6 bits because the board has 90 squares but it isn't a power of 2
  (SquareSet a) == (SquareSet b) = (a .&. unusedBitsMask) == (b .&. unusedBitsMask)
    where
      unusedBitsMask :: Word96
      unusedBitsMask = 0x3FFFFFFFFFFFFFFFFFFFFFF -- most significant 6 bits off, everything else on

instance Arbitrary SquareSet where
  -- For testing with QuickCheck
  arbitrary = SquareSet <$> (Word96 <$> arbitrary <*> arbitrary)

-- | This datatype makes it easy to work with squares on the chess board.
-- 'A1' is at the bottom left and 'I10' is at the top right of the board.
-- From this perspective, the 'Red' side will be at the bottom.
--
-- The enum instance corresponds with the bit number in the square set / bitboard.
-- For example, 'A1' has an enum value of 0 and thus will be represented by the first bit from the right in the bitboard.
--
-- The reason this isn't implemented as a combination of 'File's and 'Rank's
-- is so it's cleaner to write @A1@ instead of @Square A R1@
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
{- ORMOLU_ENABLE -}

instance Arbitrary Square where
  arbitrary = arbitraryBoundedEnum

-- | Convenience for specifying a file.
-- The A file is at the left and I is the right, from Red's perspective
data File = A | B | C | D | E | F | G | H | I
  deriving stock (Eq, Enum, Ord, Show)

-- | Convenience for specifying a rank.
-- The 1st rank is at the bottom and 10th is at the tpo, from Red's perspective
data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  deriving stock (Eq, Enum, Ord, Show)

-- }}}

-- {{{ Board
data Board = MkBoard
  { _occupied :: SquareSet,
    _red :: SquareSet,
    _black :: SquareSet,
    _rook :: SquareSet,
    _knight :: SquareSet,
    _elephant :: SquareSet,
    _advisor :: SquareSet,
    _king :: SquareSet,
    _pawn :: SquareSet,
    _cannon :: SquareSet
  }
  deriving stock (Eq, Show)

-- Do note that the instance Arbitrary definition is in Xiangqi.Board to avoid cyclic dependencies

makeLenses ''Board

-- }}}

class BoardProperty a where
  getLens :: a -> Lens' Board SquareSet

-- {{{ Role
data Role = Rook | Knight | Elephant | Advisor | King | Pawn | Cannon
  deriving stock (Eq, Show, Enum, Bounded)

instance Arbitrary Role where
  arbitrary = arbitraryBoundedEnum

instance BoardProperty Role where
  getLens Rook = rook
  getLens Knight = knight
  getLens Elephant = elephant
  getLens Advisor = advisor
  getLens King = king
  getLens Pawn = pawn
  getLens Cannon = cannon

-- }}}

-- {{{ Side

-- | The side of the piece/player.
data Side = Red | Black deriving stock (Eq, Show, Enum, Bounded)

instance Arbitrary Side where
  arbitrary = arbitraryBoundedEnum

instance BoardProperty Side where
  getLens Red = red
  getLens Black = black

-- }}}

-- {{{ Piece
data Piece = Piece
  { _role :: Role,
    _side :: Side
  }
  deriving stock (Eq, Show)

makeLenses ''Piece

instance Arbitrary Piece where
  arbitrary = Piece <$> arbitrary <*> arbitrary

-- }}}
