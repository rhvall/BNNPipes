----
---- BNN-Pipes, Copyright (C) 1/Jan/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE TemplateHaskell #-}

module Evol.SelectBNPTests
-- (
--
-- )
where

import           BNN.BNN16                (BNN16, BNNLayer)
import           BNN.BNP16                (BNP16, queryBNP16)
import           BNN.BNU16                (BNU16, BNU16Type)
import           BNN.BNUEvo               (mutateBType)
import           Control.Arrow            ((***))
import           Control.Monad.Random.Strict     (Rand, StdGen, evalRand, mkStdGen,
                                           next, split)
import           Data.Bits                ((.&.))
import           Data.Bool                (bool)
import           Data.Maybe               (fromMaybe, isJust, isNothing)
import qualified Data.Vector              as V
import           Evol.EvoBNN              (PopInfo (..), fitnessNet,
                                           runEvolution)
import           Evol.SelectBNP
import           QuickCheck.ArbitraryDefs ()
import           Test.HUnit               (Assertion, (@?=))
import           Test.QuickCheck          (NonNegative, Positive, Property,
                                           discard, getNonNegative, getPositive,
                                           (===))
import           Test.Tasty               (TestTree)
import           Test.Tasty.HUnit         (testCase)
import           Test.Tasty.QuickCheck    (testProperty)
import           Test.Tasty.TH            (testGroupGenerator)

prop_updateBNNSame :: BNN16 -> (NonNegative Int, NonNegative Int, NonNegative Int) -> StdGen -> Bool
prop_updateBNNSame bnn w@(x, y, z) seed
    | V.null bnn = discard
    | xNP >= V.length bnn = discard
    | yNP >= V.length (bnn V.! xNP) = discard
    | zNP >= V.length (bnn V.! xNP V.! yNP) = discard
    | V.null (bnn V.! xNP) = discard
    | otherwise = w == u'
    where (xNP, yNP, zNP) = (getNonNegative x, getNonNegative y, getNonNegative z)
          bnn' = evalRand (updateBNN bnn (xNP, yNP, zNP)) seed
          w = bnn' V.! xNP V.! yNP V.! zNP
          u = bnn  V.! xNP V.! yNP V.! zNP
          u' = evalRand (mutateBType u) seed

-- NOTE!! This property is hard to test as a property given that (xp, yp, zp)
-- depend on the generated bnn to work properly, sometimes it will pass other
-- it won't, an example is below that shows how to select values, however that
-- fails because random BNN16 are irregular.
prop_updateBNNDiff :: BNN16 -> (Positive Int, Positive Int, Positive Int) -> StdGen -> Bool
prop_updateBNNDiff bnn (xp, yp, zp) seed
    | not $ validIdx bnn w = discard
    | otherwise = bool (res /= res') True (V.null res)
    where (_, seed') = split seed
          w = (getPositive xp, getPositive yp, getPositive zp)
          res = evalRand (updateBNN bnn w) seed
          res' = evalRand (updateBNN bnn w) seed'
-- prop_updateBNNDiff :: BNN16 -> StdGen -> Property
-- prop_updateBNNDiff bnn seed =
--     forAll (choose (0, V.length bnn)) $ \x ->
--         forAll (choose (0, V.length (bnn V.! x))) $ \y ->
--             forAll (choose (0, V.length (bnn V.! x V.! y))) $ \z ->
--                 let w = (x, y, z)
--                     res = evalRand (updateBNN bnn w) seed
--                     res' = evalRand (updateBNN bnn w) seed'
--                 in bool (res /= res') True (V.null res)
--     where (_, seed') = split seed

prop_rdmIdxesValid :: BNN16 -> Int -> StdGen -> Bool
prop_rdmIdxesValid bnn size seed = checkIdxes vIdx
    where vIdx = evalRand (rdmIdxes bnn size) seed
          checkIdxes = V.foldl (.&.) True . V.map (validIdx bnn)

prop_createPopByMutDiff :: BNN16 -> Positive Int -> StdGen -> Bool
prop_createPopByMutDiff bnn sizeNN seed
    | V.null bnn = discard
    | V.length bnn == 1 = discard
    | otherwise = bool (pop /= pop') discard (V.null pop)
    where (_, seed') = split seed
          size = getPositive sizeNN
          pop = evalRand (createPopByMut bnn size) seed
          pop' = evalRand (createPopByMut bnn size) seed'

prop_rdmIdxValid :: BNN16 -> StdGen -> Bool -- (Maybe (Int, Int))
prop_rdmIdxValid bnn seed
    | V.null bnn = discard
    | V.null $ V.head bnn = discard
    | V.null . V.head $ V.head bnn  = discard
    | otherwise = fromMaybe discard check
    where idxM = evalRand (rdmIdx bnn maxNumTries) seed
          check = fmap (validIdx bnn) idxM

case_rdmIdxMaxTries :: Assertion
case_rdmIdxMaxTries = do
    let maxT = 5 :: BNU16Type
    let bnn = V.singleton $ V.singleton V.empty
    let expected = Nothing
    let seed = mkStdGen 2
    -- To be tested
    let bnnQ = evalRand (rdmIdx bnn maxT) seed
    -- Assertion
    bnnQ @?= expected
    -- End test

tests :: TestTree
tests = $(testGroupGenerator)

--------------------------------------------------------------------------------
--------------------------Helper Methods----------------------------------------
--------------------------------------------------------------------------------

checkVal ::  V.Vector a -> Int -> Bool
checkVal b z = isJust $ b V.!? z

validIdx :: BNN16 -> (Int, Int, Int) -> Bool
validIdx bnn (x, y, z) = xc && yc && zc
    where bnl = fromMaybe V.empty $ bnn V.!? x
          bnu = fromMaybe V.empty $ bnl V.!? y
          xc = checkVal bnn x
          yc = checkVal bnl y
          zc = checkVal bnu z
