----
---- BNN-Pipes, Copyright (C) 20/Sep/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

module Main where

import qualified BNN.BNN16Tests
import qualified BNN.BNNTests
import qualified BNN.BNPTests
import qualified BNN.BNU16Tests
import qualified BNN.BNUTests
import qualified Evol.SelectBNPTests
import           Test.Tasty          (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "MainTests"
    [
        BNN.BNUTests.tests,
        BNN.BNNTests.tests,
        BNN.BNPTests.tests,
        BNN.BNU16Tests.tests,
        BNN.BNN16Tests.tests,
        Evol.SelectBNPTests.tests
    ]

-- {-
-- Properties in comments are not run:
-- prop_comment :: Assertion
-- prop_comment = assertFailure "property in comment should not be run"
-- -}
--
-- prop_lengthAppend :: [Int] -> [Int] -> Bool
-- prop_lengthAppend as bs = length (as ++ bs) == length as + length bs
--
-- case_length_1 :: Assertion
-- case_length_1 = 1 @=? length [()]
--
-- test_plus :: [TestTree]
-- test_plus =
--   [ testCase "3 + 4" (7 @=? (3 + 4))
--     -- ...
--   ]

-- import           Test.Tasty
-- import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck as QC
-- import           Test.Tasty.SmallCheck as SC
--
-- import           Data.List
-- import           Data.Ord
--
-- main = defaultMain tests
--
-- tests :: TestTree
-- tests = testGroup "Tests" [properties, unitTests]
--
-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]
--
-- scProps = testGroup "(checked by SmallCheck)"
--   [ SC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , SC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , SC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
--   ]
--
-- qcProps = testGroup "(checked by QuickCheck)"
--   [ QC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , QC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , QC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--   ]
--
-- unitTests = testGroup "Unit tests"
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT
--
--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]
