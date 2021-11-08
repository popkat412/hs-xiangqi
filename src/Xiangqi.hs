{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

module Xiangqi
  ( -- * Move generators
    pawnMoves,
    knightMoves,
    elephantMoves,
    kingMoves,
    advisorMoves,
    rookMoves,
    cannonMoves,
  )
where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import GHC.List (foldl')
import SquareSet

-- {{{ Move generation

-- TODO: Memoize

pawnMoves ::
  Side ->
  Square ->
  SquareSet
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

rookMoves ::
  -- | Square the rook is on
  Square ->
  -- | Occupied squares
  SquareSet ->
  -- | Rook moves
  SquareSet
rookMoves sq occupied = foldl' (\acc x -> acc `union` rayAttacksWithBlockers x) empty [North, East, South, West]
  where
    rayAttacksWithBlockers dir =
      let attacks = rayAttacks dir sq
          blockers = attacks `intersection` occupied

          bitScan = if offset dir < 0 then bitScanReverse else bitScanForward

          sq' = bitScan blockers
       in maybe
            attacks
            (\x -> attacks `xor` rayAttacks dir x)
            sq'

cannonMoves ::
  -- | Square the cannon is on
  Square ->
  -- | Occupied squares
  SquareSet ->
  -- | Cannon moves
  SquareSet
cannonMoves sq occupied = foldl' (\acc x -> acc `union` cannonMoves' x) empty [North, East, South, West]
  where
    cannonMoves' dir = fromMaybe empty $ do
      -- Sliding attacks
      let attacks = rayAttacks dir sq
          blockers = attacks `intersection` occupied
          bitScan = if offset dir < 0 then bitScanReverse else bitScanForward
          blockedSq = bitScan blockers
          slidingAttacks = maybe attacks (\x -> attacks `xor` rayAttacks dir x) blockedSq

      -- Jump attacks
      let jumpAttacks' =
            let clearFn = if offset dir < 0 then clearMS1B else clearLS1B
             in do
                  jumpAttackSquare <- clearFn blockers
                  bitScan jumpAttackSquare

      let jumpAttacks = maybe empty (\x -> empty & setBit x) jumpAttacks'

      -- Combine both
      return $ slidingAttacks `union` jumpAttacks

-- {{{ Helpers

-- | Helper function to generate moves for a blockable piece, aka Knight and Elephant
blockablePieceMoves ::
  (BoardOffset a) =>
  -- | Contains info about which squares are reachable and are blocked by what other square.
  -- The 1st tuple item is the blocked square and the 2nd tuple item is the target square.
  [(CompassDir, a)] ->
  PieceContext ->
  Square ->
  SquareSet
blockablePieceMoves info context sq = foldl' fn empty info
  where
    fn acc (block, targetDir) =
      if getBitAtDir block context
        then acc
        else maybe acc (`setBit` acc) (sq `shiftSquare` targetDir)

-- | Helper function to generate moves for a piece inside the palace, aka King and Advisor
palacePieceMoves :: (BoardOffset a) => [a] -> Side -> Square -> SquareSet
palacePieceMoves info side sq = foldl' fn empty info `intersection` palaceMask side
  where
    fn acc dir = maybe acc (`setBit` acc) (sq `shiftSquare` dir)

-- TODO: Memoize
rayAttacks :: CompassDir -> Square -> SquareSet
rayAttacks dir = go empty
  where
    go ss sq' = maybe ss (\x -> go (setBit x ss) x) (sq' `shiftSquare` dir)

-- }}}

-- }}}
