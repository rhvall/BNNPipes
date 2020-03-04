----
---- BNN-Pipes, Copyright (C) 11/Nov/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE TemplateHaskell #-}

module BNN.BNPTests
-- (
--
-- )
where

import           BNN.BNP
import           BNN.BNU
import qualified Data.Vector      as V
import           Test.HUnit       (Assertion, (@?=))
import           Test.Tasty       (TestTree)
import           Test.Tasty.HUnit (testCase)
import           Test.Tasty.TH    (testGroupGenerator)


case_queryBNP1 :: Assertion
case_queryBNP1 = do
    let weights0 = V.fromList [194,170,244,58,24,152,15,163]
    let thresholds0 = V.fromList [3,3,4,5,1,4,5,1]
    let bnn0 = BNU weights0 thresholds0
    let weights1 = V.fromList [209,194,206,154,59,102,102,7]
    let thresholds1 = V.fromList [3,6,4,5,1,0,7,1]
    let bnn1 = BNU weights1 thresholds1
    let weights2 = V.fromList [81,104,238,82,57,170,102,37]
    let thresholds2 = V.fromList [0,1,4,3,1,3,7,1]
    let bnn2 = BNU weights2 thresholds2
    let bnp = V.fromList [bnn0, bnn1, bnn2]
    let input = 9
    let expected = 191
    -- To be tested
    let bnnQ = queryBNP bnp input
    -- Assertion
    bnnQ @?= expected
    -- End test

tests :: TestTree
tests = $(testGroupGenerator)
