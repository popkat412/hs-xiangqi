module Xiangqi.Game
  ( -- * Game

    -- ** Constants
    initialGame,

    -- ** Moves
    move,
    makeMove,
    makeMove',
    unmakeMove,
    unmakeMove',
  )
where

import Control.Monad.State
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl (use, (%=), (.=))
import Xiangqi.Board
import Xiangqi.Types

initialGame :: Game
initialGame =
  Game
    { _gameBoard = startingPosition,
      _currSide = Red,
      _history = []
    }

-- | Make a 'Move' with no captured piece
move :: Square -> Square -> Move
move from to = Move {_fromSq = from, _toSq = to, _capturedPiece = Nothing}

makeMove :: Move -> State Game Move
makeMove mv = do
  history %= (++ [mv])
  currSide %= toggleSide

  board <- use gameBoard
  let (modifiedMove, modifiedBoard) = runState modifyBoard board
  gameBoard .= modifiedBoard
  return modifiedMove
  where
    modifyBoard :: State Board Move
    modifyBoard = do
      pieceToMove <- takePieceAt (mv ^. fromSq)
      case pieceToMove of
        Nothing -> return mv -- do nothing if trying to move a piece that isn't there
        Just piece -> do
          capturedPiece' <- setPieceAt (mv ^. toSq) piece
          return $ mv & capturedPiece .~ capturedPiece'

makeMove' :: Move -> Game -> (Move, Game)
makeMove' mv = runState (makeMove mv)

unmakeMove :: Move -> State Game ()
unmakeMove mv = do
  history %= init
  currSide %= toggleSide

  initialBoard <- use gameBoard
  let board = execState modifyBoard initialBoard
  gameBoard .= board
  return ()
  where
    modifyBoard :: State Board ()
    modifyBoard = do
      pieceToUnmove <- takePieceAt (mv ^. toSq)
      case pieceToUnmove of
        Nothing -> return ()
        Just piece -> do
          _ <- setPieceAt (mv ^. fromSq) piece
          case mv ^. capturedPiece of
            Nothing -> return ()
            Just capturedPiece' -> do
              _ <- setPieceAt (mv ^. toSq) capturedPiece'
              return ()

unmakeMove' :: Move -> Game -> Game
unmakeMove' mv = execState (unmakeMove mv)
