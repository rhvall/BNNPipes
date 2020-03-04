----
---- BNN-Pipes, Copyright (C) 22/Sep/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE TemplateHaskell #-}

module BNN.BNUTests
-- (
--
-- )
where

import           BNN.BNU
import           Data.Bool        (bool)
import qualified Data.Vector      as V
import           Test.HUnit       (Assertion, (@?=))
import           Test.Tasty       (TestTree)
import           Test.Tasty.HUnit (testCase)
import           Test.Tasty.TH    (testGroupGenerator)

case_QueryBNU01 :: Assertion
case_QueryBNU01 = do
    let input = 9
    let thresholds = V.fromList [3,3,4,5,1,4,5,1]
    let weights = V.fromList [194,170,244,58,24,152,15,163]
    let bnn = BNU weights thresholds
    let expected = 151
    -- To be tested
    let bnnQ = queryBNU bnn input
    -- Assertion
    bnnQ @?= expected
    -- End test

case_QueryBNU02 :: Assertion
case_QueryBNU02 = do
    let input = 151
    let thresholds = V.fromList [3,6,4,5,1,0,7,1]
    let weights = V.fromList [209,194,206,154,59,102,102,7]
    let bnn = BNU weights thresholds
    let expected = 181
    -- To be tested
    let bnnQ = queryBNU bnn input
    -- Assertion
    bnnQ @?= expected
    -- End test

case_Bool2BNUBType01 :: Assertion
case_Bool2BNUBType01 = do
    let ls = [1,0,0,0,1,0,1,0]
    let lo = map (\x -> bool False True (x == 1))
    let ll = V.fromList $ lo ls
    let expected = 81
    -- To be tested
    let bnnQ = bool2BNUBType ll
    -- Assertion
    bnnQ @?= expected
    -- End test

case_Bool2BNUBType02 :: Assertion
case_Bool2BNUBType02 = do
    let ls = [0,1,0,1,1,0,0,1]
    let lo = map (\x -> bool False True (x == 1))
    let ll = V.fromList $ lo ls
    let expected = 154
    -- To be tested
    let bnnQ = bool2BNUBType ll
    -- Assertion
    bnnQ @?= expected
    -- End test

case_DeltaWeight01 :: Assertion
case_DeltaWeight01 = do
    let out = 181 :: BNUBType
    let delta = 228 :: BNUBType
    let xorWeights = V.fromList ([70,85,89,13,172,241,241,144] :: [BNUBType])
    let expected = V.fromList ([0,0,89,0,0,241,14,144] :: [BNUBType])
    -- To be tested
    let bnnQ = deltaWeight xorWeights out delta
    -- Assertion
    bnnQ @?= expected
    -- End test

case_DeltaWeight02 :: Assertion
case_DeltaWeight02 = do
    let out = 191 :: BNUBType
    let delta = 238 :: BNUBType
    let xorWeights = V.fromList ([228,221,91,231,140,31,211,144] :: [BNUBType])
    let expected = V.fromList ([0,221,91,231,0,31,44,144] :: [BNUBType])
    -- To be tested
    let bnnQ = deltaWeight xorWeights out delta
    -- Assertion
    bnnQ @?= expected
    -- End test

case_DeltaWeight03 :: Assertion
case_DeltaWeight03 = do
    let out = 255 :: BNUBType
    let delta = 238 :: BNUBType
    let xorWeights = V.fromList ([191,191,191,191,191,191,191,191] :: [BNUBType])
    let expected = V.fromList ([0,191,191,191,0,191,191,191] :: [BNUBType])
    -- To be tested
    let bnnQ = deltaWeight xorWeights out delta
    -- Assertion
    bnnQ @?= expected
    -- End test

case_DeltaThreshold01 :: Assertion
case_DeltaThreshold01 = do
    let out = 151
    let delta = 228
    let thresholds = V.fromList [3,3,4,5,1,4,5,1]
    let expected = V.fromList [3,3,5,5,1,3,4,2]
    -- To be tested
    let bnnQ = deltaThreshold thresholds out delta
    -- Assertion
    bnnQ @?= expected
    -- End test

case_DeltaThreshold02 :: Assertion
case_DeltaThreshold02 = do
    let out = 181
    let delta = 228
    let thresholds = V.fromList [3,6,4,5,1,0,7,1]
    let expected = V.fromList [3,6,5,5,1,1,6,2]
    -- To be tested
    let bnnQ = deltaThreshold thresholds out delta
    -- Assertion
    bnnQ @?= expected
    -- End test

case_DeltaThreshold'01 :: Assertion
case_DeltaThreshold'01 = do
    let expected = V.fromList [RemainT,IncreaseT,IncreaseT,RemainT,RemainT,DecreaseT,RemainT,IncreaseT]
    -- To be tested
    let bnnQ = deltaThreshold' 151 166
    -- Assertion
    bnnQ @?= expected
    -- End test

case_ApplyDeltaWeight01 :: Assertion
case_ApplyDeltaWeight01 = do
    let deltaW = V.fromList [0,163,253,204,0,110,249,170] :: V.Vector BNUBType
    let weights = V.fromList [194,170,244,58,24,152,15,163] :: V.Vector BNUBType
    let lastWU = 2 :: BNUBType
    let expected = (V.fromList [194,170,252,50,24,144,7,171], 3)
    -- To be tested
    let bnnQ = applyDeltaWeight deltaW weights lastWU
    -- Assertion
    bnnQ @?= expected
    -- End test

case_ApplyDeltaWeight02 :: Assertion
case_ApplyDeltaWeight02 = do
    let deltaW = V.fromList [0,170,89,242,0,241,14,144] :: V.Vector BNUBType
    let weights = V.fromList [209,194,206,154,59,102,102,7] :: V.Vector BNUBType
    let lastWU = 32 :: BNUBType
    let expected = (V.fromList [209,194,207,154,59,103,102,7], 0)
    -- To be tested
    let bnnQ = applyDeltaWeight deltaW weights lastWU
    -- Assertion
    bnnQ @?= expected
    -- End test

case_ApplyDeltaWeight03 :: Assertion
case_ApplyDeltaWeight03 = do
    let deltaW = V.fromList [0,191,191,191,0,191,191,191] :: V.Vector BNUBType
    let weights = V.fromList [0,0,0,0,0,0,0,0] :: V.Vector BNUBType
    let lastWU = 6 :: BNUBType
    let expected = (V.fromList [0,128,128,128,0,128,128,128], 7)
    -- To be tested
    let bnnQ = applyDeltaWeight deltaW weights lastWU
    -- Assertion
    bnnQ @?= expected
    -- End test

case_ApplyDeltaWeight04 :: Assertion
case_ApplyDeltaWeight04 = do
    let deltaW = V.fromList [0,191,191,191,0,191,191,191] :: V.Vector BNUBType
    let weights = V.fromList [129,192,107,212,123,21,129,129] :: V.Vector BNUBType
    let lastWU = 6 :: BNUBType
    let expected = (V.fromList [129,64,235,84,123,149,1,1], 7)
    -- To be tested
    let bnnQ = applyDeltaWeight deltaW weights lastWU
    -- Assertion
    bnnQ @?= expected
    -- End test

case_ApplyDeltaThreshold01 :: Assertion
case_ApplyDeltaThreshold01 = do
    let lastTU = 4 :: BNUBType
    let deltaT = deltaThreshold' 151 166
    let weights = V.fromList [3,3,4,5,1,4,5,1] :: V.Vector BNUBType
    let expected = (V.fromList [3,3,4,5,1,3,5,1], 5)
    -- To be tested
    let bnnQ = applyDeltaThreshold deltaT weights lastTU
    -- Assertion
    bnnQ @?= expected
    -- End test

case_UpdateBNU01 :: Assertion
case_UpdateBNU01 = do
    let weights = V.fromList [194,170,244,58,24,152,15,163]
    let thresholds = V.fromList [3,3,4,5,1,4,5,1]
    let update = Threshold
    let lastW = 5
    let lastT = 2
    let input = 9
    let output = 151
    let delta = 238
    let expW = V.fromList [194,170,180,122,24,216,79,163]
    let expected = (BNU expW thresholds, BNUUpdate Weight 6 lastT)
    -- To be tested
    let bnnQ = updateBNU (BNU weights thresholds) (BNUUpdate update lastW lastT) input output delta
    -- Assertion
    bnnQ @?= expected
    -- End test

case_UpdateBNU02 :: Assertion
case_UpdateBNU02 = do
    let weights = V.fromList [194,170,244,58,24,152,15,163]
    let thresholds = V.fromList [3,3,4,5,1,4,5,1]
    let update = Weight
    let lastW = 4
    let lastT = 4
    let input = 9
    let output = 151
    let delta = 166
    let expT = V.fromList [3,3,4,5,1,3,5,1]
    let expected = (BNU weights expT, BNUUpdate Threshold lastW 5)
    -- To be tested
    let bnnQ = updateBNU (BNU weights thresholds) (BNUUpdate update lastW lastT) input output delta
    -- Assertion
    bnnQ @?= expected
    -- End test

case_UpdateBNU03 :: Assertion
case_UpdateBNU03 = do
    let weights = V.fromList [45,30,29,157,254,143,47,111]
    let thresholds = V.fromList [6,1,1,6,3,7,7,5]
    let update = Weight
    let lastW = 12
    let lastT = 8
    let input = 212
    let output = 215
    let delta = 199
    let expT = V.fromList [7,1,1,6,3,7,7,5]
    let expected = (BNU weights expT, BNUUpdate Threshold lastW 0)
    -- To be tested
    let bnnQ = updateBNU (BNU weights thresholds) (BNUUpdate update lastW lastT) input output delta
    -- Assertion
    bnnQ @?= expected
    -- End test

case_UpdateBNU04 :: Assertion
case_UpdateBNU04 = do
    let weights = V.fromList [0,0,255,0,254,0,0,0]
    let thresholds = V.fromList [7,0,8,1,7,9,7,7]
    let update = Threshold
    let lastW = 0
    let lastT = 5
    let input = 0
    let output = 22
    let delta = 196
    let expW = V.fromList [0,0,253,0,254,0,2,2]
    let expected = (BNU expW thresholds, BNUUpdate Weight 1 lastT)
    -- To be tested
    let bnnQ = updateBNU (BNU weights thresholds) (BNUUpdate update lastW lastT) input output delta
    -- Assertion
    bnnQ @?= expected
    -- End test

case_UpdateBNU05 :: Assertion
case_UpdateBNU05 = do
    let weights = V.fromList [129,192,107,212,123,21,129,129]
    let thresholds = V.fromList [7,0,5,3,6,7,7,7]
    let update = Threshold
    let lastW = 3
    let lastT = 2
    let input = 64
    let output = 10
    let delta = 34
    let expW = V.fromList [129,192,107,212,123,53,129,129]
    let expected = (BNU expW thresholds, BNUUpdate Weight 5 lastT)
    -- To be tested
    let bnnQ = updateBNU (BNU weights thresholds) (BNUUpdate update lastW lastT) input output delta
    -- Assertion
    bnnQ @?= expected
    -- End test

case_UpdateBNU06 :: Assertion
case_UpdateBNU06 = do
    let weights = V.fromList [13,218,26,222,95,210,95,218]
    let thresholds = V.fromList [3,7,5,8,7,6,7,7]
    let update = Weight
    let lastW = 3
    let lastT = 5
    let input = 1
    let output = 0
    let delta = 123
    let expT = V.fromList [3,7,5,8,7,6,6,7]
    let expected = (BNU weights expT, BNUUpdate Threshold lastW 6)
    -- To be tested
    let bnnQ = updateBNU (BNU weights thresholds) (BNUUpdate update lastW lastT) input output delta
    -- Assertion
    bnnQ @?= expected
    -- End test

tests :: TestTree
tests = $(testGroupGenerator)
    -- testGroup "BNN.BNUTests"
    -- [
    --     testCase "test_QueryBNU01" test_QueryBNU01
    -- ]
