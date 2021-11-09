{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Xiangqi
  ( -- * Pieces
    Piece (..),
    Role (..),

    -- * Board
    Board,

    -- ** Constants
    startingPosition,

    -- ** Getting board data
    getRoleSS,
    getSideAt,
    getRoleAt,
    getPieceAt,

    -- ** Misc
    prettyBoard,
  )
where

import Data.Char (toLower)
import Data.Maybe (fromJust)
import SquareSet

-- {{{ Pieces
data Role = Rook | Knight | Elephant | Advisor | King | Pawn | Cannon
  deriving stock (Eq, Show, Enum, Bounded)

data Piece = Piece
  { role :: Role,
    side :: Side
  }
  deriving stock (Eq, Show)

-- | Show a 'Piece' as a single character.
-- Red pieces are in uppercase, Black pieces are in lowercase.
-- @
-- [ (Rook    , 'R'),
--   (Knight  , 'N'),
--   (Elephant, 'E'),
--   (Advisor , 'A'),
--   (King    , 'K'),
--   (Pawn    , 'P'),
--   (Cannon  , 'C')
-- ]
-- @
{- ORMOLU_DISABLE -}
compactPiece :: Piece -> Char
compactPiece (Piece role side) =
  let chars =
        [ (Rook    , 'R'),
          (Knight  , 'N'),
          (Elephant, 'E'),
          (Advisor , 'A'),
          (King    , 'K'),
          (Pawn    , 'P'),
          (Cannon  , 'C')
        ]
      letter = fromJust $ lookup role chars -- if this is Nothing, theres a bug
   in if side == Red then letter else toLower letter
{- ORMOLU_ENABLE -}

-- }}}

-- {{{ Board
data Board = Board
  { occupied :: SquareSet,
    red :: SquareSet,
    black :: SquareSet,
    rook :: SquareSet,
    knight :: SquareSet,
    elephant :: SquareSet,
    advisor :: SquareSet,
    king :: SquareSet,
    pawn :: SquareSet,
    cannon :: SquareSet
  }
  deriving stock (Eq, Show)

-- {{{ Constants

-- |
-- >>> putStrLn $ prettyBoard startingPosition
-- 10   r n e a k a e n r
-- 9    . . . . . . . . .
-- 8    . c . . . . . c .
-- 7    p . p . p . . . p
-- 6    . . . . . . . . .
-- 5    . . . . . . . . .
-- 4    P . P . P . P . P
-- 3    . C . . . . . C .
-- 2    . . . . . . . . .
-- 1    R N E A K A E N R
-- <BLANKLINE>
--      A B C D E F G H I
startingPosition :: Board
startingPosition = Board {..}
  where
    rook = SquareSet 0x20200000000000000000101
    knight = SquareSet 0x10400000000000000000082
    elephant = SquareSet 0x8800000000000000000044
    advisor = SquareSet 0x5000000000000000000028
    king = SquareSet 0x2000000000000000000010
    pawn = SquareSet 0x5540000aa8000000
    cannon = SquareSet 0x410000000002080000

    occupied = rook `union` knight `union` elephant `union` advisor `union` king `union` pawn `union` cannon

    red = occupied `intersection` redMask
    black = occupied `intersection` blackMask

-- }}}

-- {{{ Getting board data

-- | Helper to get the 'SquareSet' of a particular 'Role' from the board.
getRoleSS :: Role -> Board -> SquareSet
getRoleSS Rook = rook
getRoleSS Knight = knight
getRoleSS Elephant = elephant
getRoleSS Advisor = advisor
getRoleSS King = king
getRoleSS Pawn = pawn
getRoleSS Cannon = cannon

-- | Get the side of the piece at a square.
-- Is 'Nothing' if there is no piece at that square.
getSideAt :: Square -> Board -> Maybe Side
getSideAt sq board
  | getBit sq (red board) = Just Red
  | getBit sq (black board) = Just Black
  | otherwise = Nothing

-- | Get the Role of the piece at a square.
-- Is 'Nothing' if there is no piece at that square.
getRoleAt :: Square -> Board -> Maybe Role
getRoleAt sq board = go [minBound ..]
  where
    go [] = Nothing
    go (x : xs) =
      if getBit sq (getRoleSS x board)
        then Just x
        else go xs

-- | Get the 'Piece' at a square
getPieceAt :: Square -> Board -> Maybe Piece
getPieceAt sq board = do
  side <- getSideAt sq board
  role <- getRoleAt sq board
  return $ Piece {..}

-- }}}

-- {{{ Misc

-- | Pretty print a 'Board' for debugging
prettyBoard :: Board -> String
prettyBoard board =
  genericBoardPrettyPrinter [maybePieceToChar $ getPieceAt i board | i <- [I10, H10 .. A1]]
  where
    maybePieceToChar = maybe '.' compactPiece

-- }}}

-- }}}
