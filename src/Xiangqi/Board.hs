{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Xiangqi.Board
  ( -- * Board

    -- ** Constants
    emptyPosition,
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
import Lens.Micro
import Lens.Micro.Mtl
import Test.QuickCheck (Arbitrary (arbitrary))
import Xiangqi.SquareSet
import Xiangqi.Types

-- {{{ Board

-- Screw orphaned instances, this is within the same library dammit
-- I'm only doing orphaned instances for this one because it requires
-- other helper functions like 'union' and 'intersection', and doing this in
-- Types.hs would cause cyclic dependencies
instance Arbitrary Board where
  arbitrary = do
    _rook <- arbitrary
    _knight <- arbitrary
    _elephant <- arbitrary
    _advisor <- arbitrary
    _king <- arbitrary
    _pawn <- arbitrary
    _cannon <- arbitrary

    let _occupied = _rook `union` _knight `union` _elephant `union` _advisor `union` _king `union` _pawn `union` _cannon
        _red = _occupied `intersection` redMask
        _black = _occupied `intersection` blackMask

    return $ MkBoard {..}

instance Show Board where
  show = prettyBoard

-- {{{ Constants

-- |
-- >>> putStrLn $ prettyBoard empty
-- 10   . . . . . . . . .
-- 9    . . . . . . . . .
-- 8    . . . . . . . . .
-- 7    . . . . . . . . .
-- 6    . . . . . . . . .
-- 5    . . . . . . . . .
-- 4    . . . . . . . . .
-- 3    . . . . . . . . .
-- 2    . . . . . . . . .
-- 1    . . . . . . . . .
-- <BLANKLINE>
--      A B C D E F G H I
emptyPosition :: Board
emptyPosition =
  MkBoard
    { _rook = empty,
      _knight = empty,
      _elephant = empty,
      _advisor = empty,
      _king = empty,
      _pawn = empty,
      _cannon = empty,
      _red = empty,
      _black = empty,
      _occupied = empty
    }

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
    _rook = SquareSet 0x20200000000000000000101
    _knight = SquareSet 0x10400000000000000000082
    _elephant = SquareSet 0x8800000000000000000044
    _advisor = SquareSet 0x5000000000000000000028
    _king = SquareSet 0x2000000000000000000010
    _pawn = SquareSet 0x5540000aa8000000
    _cannon = SquareSet 0x410000000002080000

    _occupied = _rook `union` _knight `union` _elephant `union` _advisor `union` _king `union` _pawn `union` _cannon

    _red = _occupied `intersection` redMask
    _black = _occupied `intersection` blackMask

-- }}}

-- {{{ Getting board data

-- | Get the side of the piece at a square.
-- Is 'Nothing' if there is no piece at that square.
getSideAt :: Square -> StateT Board Maybe Side
getSideAt sq = do
  board <- get
  if
      | getBit sq (board ^. red) -> return Red
      | getBit sq (board ^. black) -> return Black
      | otherwise -> lift Nothing

-- | Get the Role of the piece at a square.
-- Is 'Nothing' if there is no piece at that square.
getRoleAt :: Square -> StateT Board Maybe Role
getRoleAt sq = go [minBound ..]
  where
    go :: [Role] -> StateT Board Maybe Role
    go [] = lift Nothing
    go (x : xs) = do
      ss <- use $ getLens x
      if getBit sq ss
        then return x
        else go xs

-- | Get the 'Piece' at a square
getPieceAt :: Square -> StateT Board Maybe Piece
getPieceAt sq = do
  s <- getSideAt sq
  r <- getRoleAt sq
  return Piece {_side = s, _role = r}

-- | Returns true if the square is occupied
isOccupied :: Square -> Board -> Bool
isOccupied sq board = getBit sq (board ^. occupied)

-- }}}

-- {{{ Setting board data
takePieceAt :: Square -> StateT Board Maybe Piece
takePieceAt sq = do
  piece <- getPieceAt sq
  r <- getRoleAt sq
  s <- getSideAt sq

  getLens r %= clearBit sq
  getLens s %= clearBit sq

  return piece

setPieceAt :: Square -> Piece -> StateT Board Maybe Piece
setPieceAt sq piece = do
  oldPiece <- takePieceAt sq

  getLens (piece ^. role) %= setBit sq
  getLens (piece ^. side) %= setBit sq

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

-- {{{ Pieces

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
compactPiece (Piece r s) =
  let chars =
        [ (Rook    , 'R'),
          (Knight  , 'N'),
          (Elephant, 'E'),
          (Advisor , 'A'),
          (King    , 'K'),
          (Pawn    , 'P'),
          (Cannon  , 'C')
        ]
      letter = fromJust $ lookup r chars -- if this is Nothing, theres a bug
   in if s == Red then letter else toLower letter
{- ORMOLU_ENABLE -}

-- }}}
