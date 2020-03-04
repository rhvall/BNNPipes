----
---- BNN-Pipes, Copyright (C) 02/Oct/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE TemplateHaskell #-}

module BNN.BNNTests
-- (
--
-- )
where

import           BNN.BNU
import qualified Data.Vector   as V
import           Test.Tasty    (TestTree)
import           Test.Tasty.TH (testGroupGenerator)

-- case_PrepareUpdateElem01 :: Assertion
-- case_PrepareUpdateElem01 = do
--     let bnuC = testDataBNUComp !! 1
--     let updateElem = UpdateElem 0 0 228 emptyBNUC
--     let queryElem = QueryElem 151 181 bnuC
--     let expected = UpdateElem 151 181 228 bnuC
--     -- To be tested
--     let result = prepareUpdateElem updateElem queryElem
--     -- Assertion
--     result @?= expected
--     -- End test
--
-- case_PrepareUpdateLst01 :: Assertion
-- case_PrepareUpdateLst01 = do
--     let reach = 2
--
--     let uElem = UpdateElem 181 191 228 emptyBNUC
--     let qLst = [
--                 QueryElem 181 191 $ testDataBNUComp !! 2,
--                 QueryElem 151 181 $ testDataBNUComp !! 1,
--                 QueryElem 9 151 $ head testDataBNUComp
--                 ]
--
--     let expected = [
--                 UpdateElem 181 191 228 $ testDataBNUComp !! 2,
--                 UpdateElem 151 181 228 $ testDataBNUComp !! 1
--                 ]
--     -- To be tested
--     let result = prepareUpdateLst reach uElem qLst
--     -- Assertion
--     result @?= expected
--     -- End test

----------------------------------------

testDataBNU :: [BNU]
testDataBNU = map toBNU [bnu0, bnu1, bnu2]
    where bnu0 = ([194,170,244,58,24,152,15,163],[3,3,4,5,1,4,5,1])
          bnu1 = ([209,194,206,154,59,102,102,7],[3,6,4,5,1,0,7,1])
          bnu2 = ([81,104,238,82,57,170,102,37],[0,1,4,3,1,3,7,1])
          toBNU (x, y) = BNU (V.fromList x) (V.fromList y)

testDataBNUXOR9 :: [BNUVType]
testDataBNUXOR9 = [bnuV0, bnuV1, bnuV2]
    where bnuV0 = V.fromList [203,163,253,51,17,145,6,170]
          bnuV1 = V.fromList [70,85,89,13,172,241,241,144]
          bnuV2 = V.fromList [228,221,91,231,140,31,211,144]

testDataBNUUpdate :: [BNUUpdate]
testDataBNUUpdate = [bnuU0, bnuU1, bnuU2]
    where bnuU0 = BNUUpdate Weight 0 1
          bnuU1 = BNUUpdate Threshold 2 5
          bnuU2 = BNUUpdate Weight 4 1

testDataBNUComp :: [BNUComp]
testDataBNUComp = map (\(x,y,z) -> BNUComp x y z) zipped
    where zipped = zip3 testDataBNU testDataBNUUpdate testDataBNUXOR9

----------------------------------------

tests :: TestTree
tests = $(testGroupGenerator)
    -- testGroup "BNN.BNUTests"
    -- [
    --     testCase "test_QueryBNU01" test_QueryBNU01
    -- ]
