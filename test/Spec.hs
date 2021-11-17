{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.State
import Data.Maybe
import Test.Invariant
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Xiangqi.Board
import qualified Xiangqi.SquareSet as SS
import Xiangqi.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

-- {{{ Unit
unitTests :: TestTree
unitTests =
  testGroup
    "UNIT TESTS"
    [ testGroup
        "Board"
        [ testCase "getPieceAt 1" $ evalStateT (getPieceAt A1) startingPosition @?= Just Piece {_role = Rook, _side = Red},
          testCase "getPieceAt 2" $ evalStateT (getPieceAt B1) startingPosition @?= Just Piece {_role = Knight, _side = Red},
          testCase "getPieceAt 3" $ evalStateT (getPieceAt A2) startingPosition @?= Nothing
        ]
    ]

-- }}}

-- {{{ Properties
properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps =
  testGroup
    "QUICKCHECK"
    [ testGroup
        "Set operations"
        -- {{{ Set operations
        [ testGroup
            "Intersection"
            [ QC.testProperty "Idempotent intersection" $
                \a -> a `SS.intersection` a == a,
              QC.testProperty "Commutative intersection" $
                commutative SS.intersection,
              QC.testProperty "Associative intersection" $
                associative SS.intersection
            ],
          testGroup
            "Union"
            [ QC.testProperty "Idempotent union" $
                \a -> a `SS.union` a == a,
              QC.testProperty "Commutative union" $
                commutative SS.union,
              QC.testProperty "Associative union" $
                associative SS.union,
              QC.testProperty "Distributive union" $
                SS.union `distributesLeftOver` SS.intersection,
              QC.testProperty "Unions in terms of xors" $
                \a b -> a `SS.union` b == (a `SS.intersection` b) `SS.xor` (a `SS.xor` b)
            ],
          testGroup
            "Complement"
            [ QC.testProperty "A' ∪ A = ξ" $
                \a -> a `SS.union` SS.complement a == SS.universal,
              QC.testProperty "A' ∩ A = ∅" $
                \a -> a `SS.intersection` SS.complement a == SS.empty,
              QC.testProperty "Same as xor with universal set" $
                SS.xor SS.universal <=> SS.complement
            ],
          testGroup
            "Xor"
            [ QC.testProperty "Commutative xor" $
                commutative SS.xor,
              QC.testProperty "Associative xor" $
                associative SS.xor,
              QC.testProperty "Distributive xor" $
                SS.intersection `distributesLeftOver` SS.xor,
              QC.testProperty "Involutory xor" $
                \a b -> (a `SS.xor` b) `SS.xor` b == a
            ]
        ],
      -- }}}
      testGroup
        "Board operations"
        -- {{{ Board operations
        [ testGroup
            "isOccupied"
            [ QC.testProperty "isOccupied should always return False if the board is empty" $
                \sq -> not (isOccupied sq emptyPosition)
            ],
          testGroup
            "getPieceAt"
            [ QC.testProperty "getPieceAt shouldn't change the board state" $
                \sq board -> let newBoard = execStateT (getPieceAt sq) board in newBoard == Just board || isNothing newBoard,
              QC.testProperty "getPieceAt should always return Nothing if the board is empty" $
                \sq -> runOnEmptyBoard (getPieceAt sq)
            ],
          testGroup
            "takePieceAt"
            [ QC.testProperty "takePieceAt should always return Nothing if the board is empty" $
                \sq -> runOnEmptyBoard (takePieceAt sq),
              QC.testProperty "takePieceAt should only change either 1 or 0 pieces" $
                \sq -> assertOnlyChangesOneOrZeroPieces (takePieceAt sq)
            ],
          testGroup
            "setPieceAt"
            [ QC.testProperty "setPieceAt should only change either 1 or 0 pieces" $
                \sq piece -> assertOnlyChangesOneOrZeroPieces (setPieceAt sq piece)
            ]
        ]
        -- }}}
    ]

-- }}}

-- {{{ Helpers
runOnEmptyBoard :: StateT Board Maybe a -> Bool
runOnEmptyBoard s = isNothing $ execStateT s emptyPosition

assertOnlyChangesOneOrZeroPieces :: StateT Board Maybe a -> Board -> Bool
assertOnlyChangesOneOrZeroPieces s board =
  let newBoard = execStateT s board
      numChanges = pieceDiffCount board $ fromMaybe board newBoard
   in numChanges == 1 || numChanges == 0

pieceDiffCount ::
  Board ->
  -- | Original
  Board ->
  -- | New
  Int -- Number of pieces that changed
pieceDiffCount orig new = foldr go 0 [minBound ..]
  where
    go :: Square -> Int -> Int
    go sq acc =
      let getPieceAt' sq' = evalStateT (getPieceAt sq')
       in if getPieceAt' sq orig == getPieceAt' sq new then acc else acc + 1

-- Check that the board is in an internally consistent state
checkInternalConsistency :: Board -> Bool
checkInternalConsistency MkBoard {..} = checkOccupied && checkRed && checkBlack
  where
    checkOccupied = _occupied == (_rook `SS.union` _knight `SS.union` _elephant `SS.union` _advisor `SS.union` _king `SS.union` _pawn `SS.union` _cannon)
    checkRed = _red == (_occupied `SS.intersection` SS.redMask)
    checkBlack = _black == (_occupied `SS.intersection` SS.blackMask)

-- }}}
