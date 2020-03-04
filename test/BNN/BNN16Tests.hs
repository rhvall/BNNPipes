----
---- BNN-Pipes, Copyright (C) 30/Dec/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE TemplateHaskell #-}

module BNN.BNN16Tests
-- (
--
-- )
where

import           BNN.BNN16
import           BNN.BNU16                (BNU16, BNU16Type, queryBNU16)
import           BNN.BNU16Random
import           Control.Monad.Random.Strict
import           Data.Bits                (xor)
import qualified Data.Vector              as V
import           QuickCheck.ArbitraryDefs ()
import           Test.HUnit               (Assertion, (@?=))
import           Test.Tasty               (TestTree)
import           Test.Tasty.HUnit         (testCase)
import           Test.Tasty.QuickCheck    (testProperty)
import           Test.Tasty.TH            (testGroupGenerator)

-- prop_queryBNN16 :: BNNInput -> BNN16 -> Bool
-- prop_queryBNN16 x y = queryBNN16 x y == z
--     where z = V.last $ V.scanl queryBNNLayer x y
--
-- prop_distanceBNN16 :: QueryVec -> BNN16 -> Bool
-- prop_distanceBNN16 x@(QueryVec inQV outQV) y = distanceBNN16 x y == z
--     where qry = queryBNN16 inQV y
--           z = V.zipWith xor qry outQV

case_queryBNN161 :: Assertion
case_queryBNN161 = do
    let input = V.fromList [1 .. 3]
        bnu0 = evalRand (randomBNU16 10) (mkStdGen 0)
        bnu1 = evalRand (randomBNU16 10) (mkStdGen 1)
        bnu2 = evalRand (randomBNU16 4) (mkStdGen 2)
        bnu3 = evalRand (randomBNU16 2) (mkStdGen 3)
        bnl0 = V.fromList [bnu0, bnu1, bnu2]
        bnl1 = V.fromList [bnu0, bnu3]
        bnn = V.fromList [bnl0, bnl1]
        expected = V.fromList [4008]
    -- To be tested
        comp = queryBNN16 input bnn
    -- Assertion
    comp @?= expected
    -- End test

case_queryBNN162 :: Assertion
case_queryBNN162 = do
    let inSize = 20
    let outSize = 10
    bnn <- evalRandIO $ randomBNN16 outSize inSize 5
    input <- V.fromList . take inSize <$> getRandomRs (0, 4096) :: IO (V.Vector BNU16Type)
    let out = queryBNN16 input bnn
    V.length out @?= outSize

tests :: TestTree
tests = $(testGroupGenerator)

--------------------------------------------------------------------------------
--------------------------Helper Methods----------------------------------------
--------------------------------------------------------------------------------

toBNNLayer :: [[BNU16Type]] -> BNNLayer
toBNNLayer = V.fromList . map V.fromList

toBNN16 :: [[[BNU16Type]]] -> BNN16
toBNN16 = V.fromList . map toBNNLayer
