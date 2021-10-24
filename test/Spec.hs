module Main (main) where

import qualified SquareSet as SS
import Test.Invariant
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps =
  testGroup
    "(checked by QuickCheck)"
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
    ]
