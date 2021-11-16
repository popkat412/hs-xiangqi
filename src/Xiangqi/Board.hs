{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Xiangqi.Board
  ( -- * Pieces
    Piece (..),
    Role (..),

    -- * Board
    Board (rook, knight, elephant, advisor, king, pawn, cannon, red, black, occupied),

    -- ** Constants
    startingPosition,

    -- ** Getting board data
    isOccupied,
    getPieceAt,

    -- ** Setting board dat
    takePieceAt,
    setPieceAt,

    -- ** Misc
    prettyBoard,
  )
where

import Control.Monad.State
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Xiangqi.Helpers (constState)
import Xiangqi.SquareSet

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
data Board = MkBoard
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
startingPosition = MkBoard {..}
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
getRoleSS :: (MonadState Board m) => Role -> m SquareSet
getRoleSS Rook = constState rook
getRoleSS Knight = constState knight
getRoleSS Elephant = constState elephant
getRoleSS Advisor = constState advisor
getRoleSS King = constState king
getRoleSS Pawn = constState pawn
getRoleSS Cannon = constState cannon

getSideSS :: (MonadState Board m) => Side -> m SquareSet
getSideSS Red = constState red
getSideSS Black = constState black

-- | Get the side of the piece at a square.
-- Is 'Nothing' if there is no piece at that square.
getSideAt :: Square -> StateT Board Maybe Side
getSideAt sq = do
  board <- get
  if
      | getBit sq (red board) -> return Red
      | getBit sq (black board) -> return Black
      | otherwise -> lift Nothing

-- | Get the Role of the piece at a square.
-- Is 'Nothing' if there is no piece at that square.
getRoleAt :: Square -> StateT Board Maybe Role
getRoleAt sq = go [minBound ..]
  where
    go [] = lift Nothing
    go (x : xs) = do
      ss <- getRoleSS x
      if getBit sq ss
        then return x
        else go xs

-- | Get the 'Piece' at a square
getPieceAt :: Square -> StateT Board Maybe Piece
getPieceAt sq = do
  side <- getSideAt sq
  role <- getRoleAt sq
  return Piece {..}

-- | Returns true if the square is occupied
isOccupied :: Square -> Board -> Bool
isOccupied sq board = getBit sq (occupied board)

-- }}}

-- {{{ Setting board data

-- TODO: Use lens
takePieceAt :: Square -> StateT Board Maybe Piece
takePieceAt sq = do
  piece <- getPieceAt sq
  role <- getRoleAt sq
  side <- getSideAt sq

  oldRoleSS <- getRoleSS role
  oldSideSS <- getSideSS side

  let newRoleSS = clearBit sq oldRoleSS
      newSideSS = clearBit sq oldSideSS

  setRoleSS role newRoleSS
  setSideSS side newSideSS

  return piece

-- TODO: see if there's a better way to do this
setRoleSS :: (MonadState Board m) => Role -> SquareSet -> m ()
setRoleSS Rook ss = modify (\board -> board {rook = ss})
setRoleSS Knight ss = modify (\board -> board {knight = ss})
setRoleSS Elephant ss = modify (\board -> board {elephant = ss})
setRoleSS Advisor ss = modify (\board -> board {advisor = ss})
setRoleSS King ss = modify (\board -> board {king = ss})
setRoleSS Pawn ss = modify (\board -> board {pawn = ss})
setRoleSS Cannon ss = modify (\board -> board {cannon = ss})

setSideSS :: (MonadState Board m) => Side -> SquareSet -> m ()
setSideSS Red ss = modify (\board -> board {red = ss})
setSideSS Black ss = modify (\board -> board {black = ss})

setPieceAt :: Square -> Piece -> StateT Board Maybe Piece
setPieceAt sq piece = do
  let role' = role piece
      side' = side piece

  oldPiece <- takePieceAt sq

  oldRoleSS <- getRoleSS role'
  oldSideSS <- getSideSS side'

  let newRoleSS = setBit sq oldRoleSS
      newSideSS = setBit sq oldSideSS

  setRoleSS role' newRoleSS
  setSideSS side' newSideSS

  return oldPiece

-- }}}

-- {{{ Misc

-- | Pretty print a 'Board' for debugging
prettyBoard :: Board -> String
prettyBoard board =
  genericBoardPrettyPrinter [maybePieceToChar $ getPieceAt' i | i <- [I10, H10 .. A1]]
  where
    getPieceAt' i = evalStateT (getPieceAt i) board
    maybePieceToChar = maybe '.' compactPiece

-- }}}

-- }}}
