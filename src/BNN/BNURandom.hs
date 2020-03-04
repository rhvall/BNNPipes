----
---- BNN-Pipes, Copyright (C) 21/Sep/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

module BNN.BNURandom
-- (
--
-- )
where

import           BNN.BNP
import           BNN.BNU
import           Data.Bool     (bool)
import qualified Data.Vector   as V
import           System.Random

randomBNU :: RandomGen g => g -> BNU
randomBNU seed = BNU (randWeight seed) (randThres seed')
    where (_, seed') = next seed
          randWeight s = V.fromList . take (fromIntegral bnuBSize) $ randomRs (minBound, maxBound) s
          randThres s = V.fromList . take (fromIntegral bnuBSize) $ randomRs (0, bnuBSize - 1) s

randomBNUUpdate :: RandomGen g => g -> BNUUpdate
randomBNUUpdate seed = BNUUpdate ut (randomVal seed') (randomVal seed'')
    where (_, seed') = next seed
          (_, seed'') = next seed'
          ut = bool Weight Threshold . fst $ random seed
          randomVal = fst . randomR (0, bnuBSize - 1)

randomBNUC :: RandomGen g => g -> BNUComp
randomBNUC seed = BNUComp bnu bnuU (bnuW bnu)
    where bnu = randomBNU seed
          bnuU = randomBNUUpdate seed

randomBNP :: RandomGen g => g -> Int -> BNP
randomBNP seed size = fst $ randomBNP' seed size

randomBNP' :: RandomGen g => g -> Int -> (BNP, g)
randomBNP' seed size = (bnpBatch, V.last seedVec)
    where seedVec = seedVector seed size
          bnpBatch = V.map randomBNU seedVec

randomBNPFromBNP :: RandomGen g => g -> Int -> BNP -> (BNPBatch, g)
randomBNPFromBNP seed size bnp = (bnuBatch, seed')
    where seedVec = seedVector seed size
          (_, seed') = next $ V.last seedVec
          bnpSize = V.length bnp
          -- rndBNPs = V.map randomBNU seedVec
          -- (val, nextSeed) = next $ V.last seedVec
          -- bnuOprs = V.fromList $ applyNTimes val shiftLeft [andBNUOpr, orBNUOpr, xorBNUOpr, xnorBNUOpr]
          bnuOprs = V.fromList [andBNPOpr, orBNPOpr, xorBNPOpr, xnorBNPOpr]
          bnuBatchOprs = V.generate size (\x -> bnuOprs V.! mod x (V.length bnuOprs))
          rdnBNPBatch = V.map (`randomBNP` bnpSize) seedVec
          bnuBatch = V.zipWith (\f y -> f bnp y) bnuBatchOprs rdnBNPBatch

seedVector :: RandomGen g => g -> Int -> V.Vector g
seedVector seed size = V.iterateN size (snd . next) seed

emptyBNU :: BNU
emptyBNU = BNU V.empty V.empty

zeroBNUUpdate :: BNUUpdate
zeroBNUUpdate = BNUUpdate Weight 0 0

emptyBNUC :: BNUComp
emptyBNUC = BNUComp emptyBNU zeroBNUUpdate V.empty

zeroBNU :: BNU
zeroBNU = BNU (V.replicate bnuBSizeInt 0) (V.replicate bnuBSizeInt (bnuBSize - 1))

zeroBNUC :: BNUComp
zeroBNUC = BNUComp zeroBNU zeroBNUUpdate V.empty
