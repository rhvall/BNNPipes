----
---- BNN-Pipes, Copyright (C) 27/Dec/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE TemplateHaskell #-}

module Evol.EvoBNNTests
-- (
--
-- )
where

import           BNN.BNN16            (BNN16, BNNInput, QueryVec (..),
                                       queryBNN16)
import           BNN.BNP16            (BNP16, queryBNP16)
import           BNN.BNU16            (BNU16Type)
import           Control.Monad.Random.Strict
--(evalRand, evalRandIO, getRandomR,,
--                                       mkStdGen, randomRs)
import           Data.Bool            (bool)
import qualified Data.Vector          as V
import           Evol.EvoBNN
import           Test.Tasty           (TestTree)
import           Test.Tasty.TH        (testGroupGenerator)

tests :: TestTree
tests = $(testGroupGenerator)

--------------------------------------------------------------------------------
----------------------BNN Evol MNIST and SIN tests------------------------------
--------------------------------------------------------------------------------

evoXOR :: IO BNP16
evoXOR = do
    let rInput = V.fromList [V.singleton 0, V.singleton 1, V.singleton 2, V.singleton 3]
        rOutput = V.fromList [V.singleton 1, V.singleton 0, V.singleton 0, V.singleton 1]
        target = V.zipWith QueryVec rInput rOutput
        popInfo = PopInfo 2 1 2 64 10 64 1024
    (info, pop) <- evalRandIO $ runEvolutionInfo fitnessNet target popInfo
    let scores = V.map (`queryBNP16` pop) rOutput
    print scores
    return pop

evoSIN :: IO BNP16
evoSIN = do
    sinSample <- evalRandIO $ sampleSIN 5
    let toBNU16 = V.singleton . fromIntegral . round . (*) 1000
        target = V.map (\(x,y) -> QueryVec (toBNU16 x) (toBNU16 y)) sinSample
        input = V.map inQV target
        popInfo = PopInfo 10 1 5 128 20 40 10024
    (info, pop) <- evalRandIO $ runEvolutionInfo fitnessNet target popInfo
    let scores = V.map (`queryBNP16` pop) input
    print scores
    return pop

evoSINLong :: IO (V.Vector QueryVec, BNP16)
evoSINLong = do
    sinSample <- evalRandIO $ sampleSIN 5
    let prec = 2
        target = V.map (\(x,y) -> QueryVec (floatToVec x prec) (floatToVec y prec)) sinSample
        input = V.map inQV target
        popInfo = PopInfo 12 4 10 128 20 40 1500
    (info, pop) <- evalRandIO $ runEvolutionInfo fitnessSINLong target popInfo
    let scores = V.map (`queryBNP16` pop) input
    print scores
    print target
    return (target, pop)

sampleSIN :: RandomGen g => Int -> Rand g (V.Vector (Float, Float))
sampleSIN size = do
    input <- V.replicateM size $ getRandomR (0, 2 * pi)
    let sinS = V.map sin input
    return $ V.zip input sinS

floatToVec :: Float -> Int -> BNNInput
floatToVec val precision = V.cons sign $ V.cons whole vec
    where sign = bool 0 1 (val >= 0)
          whole = truncate $ abs val :: BNU16Type
          frac x = x - fromInteger (floor x)
          moveFrac x = 1000 * frac x
          appender (x,y,z) = let fracY = moveFrac y in (V.snoc x (floor fracY), frac fracY, z - 1)
          (vec, _, _) = until (\(_, y, z) -> y <= 0 || z <= 0) appender (V.empty, frac $ abs val, precision)

vecToFloat :: BNNInput -> Float
vecToFloat vec
    | V.null vec = 0
    | otherwise = ff * sign
        where sign = bool (-1) 1.0 (V.head vec == 1)
              -- opr :: Int -> BNU16Type -> Float -> Float
              opr y i x = y + (10 ^^ (-3 * i) * fromIntegral x) :: Float
              ff = V.ifoldl opr 0.0 $ V.tail vec

fitnessSINLong :: FitnessFunction
fitnessSINLong queries bnn = round $ V.sum bnuOut
    where bnuOut = V.map (`distanceSINLong` bnn) queries

distanceSINLong :: QueryVec -> BNN16 -> Float
distanceSINLong (QueryVec inQV outQV) bnn = abs $ expOut - queryF
    where query = queryBNN16 inQV bnn
          expOut = vecToFloat outQV
          queryF = vecToFloat query
