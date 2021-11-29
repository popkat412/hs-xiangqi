{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Xiangqi.Board
  ( -- * Board

    -- ** Constants
    emptyPosition,
    startingPosition,

    -- ** Getting board data
    isOccupied,
    isOccupied',
    getPieceAt,
    getPieceAt',

    -- ** Setting board dat
    takePieceAt,
    takePieceAt',
    setPieceAt,
    setPieceAt',

    -- ** Misc
    prettyBoard,
    checkInternalConsistency,
  )
where

import Control.Monad.State
import Data.Char (toLower)
import Data.List (transpose)
import Data.Maybe (fromJust)
import Lens.Micro
import Lens.Micro.Mtl
import Test.QuickCheck (Arbitrary (arbitrary), Gen, frequency, vectorOf)
import Xiangqi.Helpers (count, genericBoardPrettyPrinter, showBits)
import Xiangqi.SquareSet
import Xiangqi.Types

-- {{{ Board

-- Screw orphaned instances, this is within the same library dammit
-- I'm only doing orphaned instances for this one because it requires
-- other helper functions like 'union' and 'intersection', and doing this in
-- Types.hs would cause cyclic dependencies
instance Arbitrary Board where
  arbitrary = do
    -- the board as a [Maybe Piece]. 'listBoard !! (fromEnum A1)` gives the piece at A1, and so on
    (listBoard :: [Maybe Piece]) <-
      vectorOf 90 $
        frequency [(2, return Nothing), (1, arbitrary :: Gen (Maybe Piece))]

    let f acc (Just piece, sq) = setPieceAt' sq piece acc
        f acc (Nothing, _) = acc
        board = foldl f emptyPosition $ zip listBoard [(minBound :: Square) ..]

    return board

-- instance Show Board where
--   show = prettyBoard

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
getSideAt :: Square -> State Board (Maybe Side)
getSideAt sq = do
  board <- get
  if
      | getBit sq (board ^. red) -> return $ Just Red
      | getBit sq (board ^. black) -> return $ Just Black
      | otherwise -> return Nothing

-- | Get the Role of the piece at a square.
-- Is 'Nothing' if there is no piece at that square.
getRoleAt :: Square -> State Board (Maybe Role)
getRoleAt sq = go [minBound ..]
  where
    go :: [Role] -> State Board (Maybe Role)
    go [] = return Nothing
    go (x : xs) = do
      ss <- use $ getLens x
      if getBit sq ss
        then return $ Just x
        else go xs

-- | Get the 'Piece' at a square
getPieceAt :: Square -> State Board (Maybe Piece)
getPieceAt sq = do
  (maybeSide :: Maybe Side) <- getSideAt sq
  (maybeRole :: Maybe Role) <- getRoleAt sq
  case maybeSide of
    Nothing -> return Nothing
    Just s -> case maybeRole of
      Nothing -> return Nothing
      Just r ->
        return $ Just Piece {_side = s, _role = r}

-- | Same as 'getPieceAt' but without the StateT monad
getPieceAt' :: Square -> Board -> Maybe Piece
getPieceAt' sq = evalState (getPieceAt sq)

-- | Returns true if the square is occ upied
isOccupied :: Square -> State Board Bool
isOccupied sq = do
  board <- get
  return $ getBit sq (board ^. occupied)

-- | Same as 'isOccupied' but without the StateT monad
isOccupied' :: Square -> Board -> Bool
isOccupied' sq board = getBit sq (board ^. occupied)

-- }}}

-- {{{ Setting board data
takePieceAt :: Square -> State Board (Maybe Piece)
takePieceAt sq = do
  maybePiece <- getPieceAt sq

  case maybePiece of
    Nothing -> return Nothing
    Just piece -> do
      getLens (piece ^. role) %= clearBit sq
      getLens (piece ^. side) %= clearBit sq
      occupied %= clearBit sq
      return $ Just piece

takePieceAt' :: Square -> Board -> (Maybe Piece, Board)
takePieceAt' sq = runState (takePieceAt sq)

-- | Set the piece at a square, returning the previous piece that was there.
setPieceAt :: Square -> Piece -> State Board (Maybe Piece)
setPieceAt sq piece = do
  maybeOldPiece <- takePieceAt sq

  getLens (piece ^. role) %= setBit sq
  getLens (piece ^. side) %= setBit sq
  occupied %= setBit sq

  return maybeOldPiece

setPieceAt' :: Square -> Piece -> Board -> Board
setPieceAt' sq piece = execState (setPieceAt sq piece)

-- }}}

-- {{{ Misc

-- assertInternalConsistency :: StateT Board Maybe ()
-- assertInternalConsistency = state $ \board -> assert (checkInternalConsistency board) ((), board)

-- | Pretty print a 'Board' for debugging
prettyBoard :: Board -> String
prettyBoard board =
  -- TODO: add haskell-valid reprsentation as footer
  genericBoardPrettyPrinter "" [maybePieceToChar $ getPieceAt' i board | i <- [I10, H10 .. A1]]
  where
    maybePieceToChar = maybe '.' compactPiece

-- | Check the internal consistency of the board
checkInternalConsistency :: Board -> Bool
checkInternalConsistency MkBoard {..} = checkPieces && checkRoles && checkOverlap
  where
    checkPieces = _occupied == (_rook `union` _knight `union` _elephant `union` _advisor `union` _king `union` _pawn `union` _cannon)
    checkRoles = _occupied == (_red `union` _black)
    checkOverlap = all (\s -> count (== '1') s <= 1) $ transpose $ map showBits [_rook, _knight, _elephant, _advisor, _king, _pawn, _cannon]

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
