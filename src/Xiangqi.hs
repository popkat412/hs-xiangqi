{-# LANGUAGE DerivingStrategies #-}

module Xiangqi
  ( pawnMoves,
    knightMoves,
    elephantMoves,
    kingMoves,
    advisorMoves,
  )
where

import Data.Bits ((.&.), (.|.))
import Data.Function ((&))
import GHC.List (foldl')
import SquareSet

-- {{{ Move generation
-- TODO: Memoize
pawnMoves :: Side -> Square -> SquareSet
pawnMoves side sq =
  let dir = if side == Red then North else South
      rank = getRank sq
      file = getFile sq
      acrossRiver = if side == Red then rank > R6 else rank < R5

      bb = setBit sq empty

      forward = shiftSS dir bb
      west = if file == A then empty else shiftSS West bb
      east = if file == I then empty else shiftSS East bb
   in if acrossRiver then forward `union` east `union` west else forward

{- ORMOLU_DISABLE -}
-- TODO: Memoize
knightMoves :: PieceContext -> Square -> SquareSet
knightMoves =
  blockablePieceMoves
    [ (North, KnightNNE),
      (North, KnightNNW),
      (East , KnightENE),
      (East , KnightESE),
      (South, KnightSSE),
      (South, KnightSSW),
      (West , KnightWSW),
      (West , KnightWNW)
    ]
{- ORMOLU_ENABLE -}

-- TODO: Memoize
elephantMoves :: PieceContext -> Square -> SquareSet
elephantMoves =
  blockablePieceMoves
    [ (NorthEast, ElephantNE),
      (SouthEast, ElephantSE),
      (SouthWest, ElephantSW),
      (NorthWest, ElephantNW)
    ]

kingMoves :: Side -> Square -> SquareSet
kingMoves = palacePieceMoves [North, East, South, West]

advisorMoves :: Side -> Square -> SquareSet
advisorMoves = palacePieceMoves [NorthEast, SouthEast, SouthWest, NorthWest]

-- {{{ Helpers

-- | Contains info about which squares are reachable and are blocked by what other square.
-- | The 1st tuple item is the blocked square. If the
-- | and the 2nd tuple item is the target square
type BlockablePieceInfo a = [(CompassDir, a)]

{-# INLINE blockablePieceMoves #-}
blockablePieceMoves :: (BoardDir a) => BlockablePieceInfo a -> PieceContext -> Square -> SquareSet
blockablePieceMoves info context sq = foldl' fn empty info
  where
    fn acc (block, targetDir) =
      if getBitAtDir block context
        then acc
        else case sq `shiftSquare` targetDir of
          Just x -> acc .|. (empty & setBit x)
          Nothing -> acc

{-# INLINE palacePieceMoves #-}
palacePieceMoves :: (BoardDir a) => [a] -> Side -> Square -> SquareSet
palacePieceMoves info side sq = foldl' fn empty info .&. palaceMask side
  where
    fn acc dir = case sq `shiftSquare` dir of
      Just x -> acc .|. (empty & setBit x)
      Nothing -> acc

-- }}}

-- }}}
