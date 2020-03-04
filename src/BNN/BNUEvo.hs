----
---- BNN-Pipes, Copyright (C) 23/Dec/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- Module with functions that mutate and mate BNUs and BNPs. It always
-- considers weights or threshold limits.
module BNN.BNUEvo
-- (
--
-- )
where

import           BNN.BNN16            (BNN16, BNNLayer)
import           BNN.BNP16            (BNP16)
import           BNN.BNU16            (BNU16, BNU16Type, maxThreshold,
                                       maxWeight, shiftBackTr, thresholdBits,
                                       weightBits)
import           BNN.VectorUtilities  (foldGeneric)
import           Control.Monad.Random.Strict
import           Data.Bits            (complement, complementBit, popCount,
                                       testBit, xor, (.&.), (.|.))
import           Data.Bool            (bool)
import qualified Data.Vector          as V

--------------------------------------------------------------------------------
-------------------------------- CROSSOVER -------------------------------------
--------------------------------------------------------------------------------

-- From two inputs, take the first half of X and the second from Y to
-- obtain the values in them in respect to weights (0 - 12th bit). For
-- thresholds, it increments the first by the second element up to 12, then
-- it starts to count again from 0. Example:
-- X = 12482    (0011 0000 1100 0010)
-- Y = 20538    (0101 0000 0011 1010)
-- -------------------
-- Z = 33018    (1000 0000 1111 1010)
mateBType :: BNU16Type -> BNU16Type -> BNU16Type
mateBType x y = trUpdate .|. (weight0 .|. weight1)
    where weight0 = (.&.) 4032 $ weightBits x
          weight1 = (.&.) 63   $ weightBits y
          threshold = thresholdBits x + thresholdBits y
          trUpdate = shiftBackTr $ bool threshold (threshold - 12) (threshold > 12)

-- Apply to each BNU element in the vector the function mateBType
mateBNU16 :: BNU16 -> BNU16 -> BNU16
mateBNU16 = V.zipWith mateBType

-- Apply to each BNNLayer element in the vector the function mateBNU16
mateBNNLayer :: BNNLayer -> BNNLayer -> BNNLayer
mateBNNLayer = V.zipWith mateBNU16

-- Mate two BNN16 into a single element using mateBNU16 Have in consideration
-- that order in this function matters. Example:
-- bnnX = [[[18208,29352],[27375, 2796]],[[27383,24293],[18800,20451]]]
-- bnnY = [[[8689,   495],[11023,24823]],[[51560,24457],[21714,43072]]]
-- bnnZ = [[[26417,29359],[35535,27383]],[[27368,44745],[39250,12224]]]
mateBNN16 :: BNN16 -> BNN16 -> BNN16
mateBNN16 = V.zipWith mateBNNLayer

--------------------------------------------------------------------------------
-------------------------------- MUTATION --------------------------------------
--------------------------------------------------------------------------------
-- Change bits (with xor) of a BNU16Type given a random seed. Note, if
-- the value of the threshold exceeds 12, it will be restarted to 0. Example:
-- seed = 5
-- flipBitsW = 2783     (0000 1010 1101 1111)
-- flipBitsT = 8        (0000 0000 0000 1000)
-- input     = 28774    (0111 0000 0110 0110)
-- -----------------XOR---------------------
-- Z        = 15033     (0011 1010 1011 1001)
-- weight   = 2745      (0000 1010 1011 1001)
-- threshold= 15        (0000 0000 0000 1111)
-- trUpdate = 3         (0000 0000 0000 0011)
mutateBType :: RandomGen g => BNU16Type -> Rand g BNU16Type
mutateBType x = do
    flipBitsW <- getRandomR (0, maxWeight)
    flipBitsT <- getRandomR (0, maxThreshold)
    let weight = xor (weightBits x) flipBitsW
    let threshold = xor (thresholdBits x) flipBitsT
    let trUpdate = shiftBackTr $ bool threshold (threshold - 12) (threshold > 12)
    return $ trUpdate .|. weight

-- Apply function "mutateBType" to each element in the BNU16 using
-- MonadRandom. Example:
-- bnu = [47477,14510]
-- result = [32626,24313]
mutateBNU16 :: RandomGen g => BNU16 -> Rand g BNU16
mutateBNU16 bnu = V.forM bnu mutateBType

-- Apply function "mutateBNU16" to each element in the BNNLayer
mutateBNNLayer :: RandomGen g => BNNLayer -> Rand g BNNLayer
mutateBNNLayer bnl = V.forM bnl mutateBNU16

-- Apply function "mutateBNNLayer" to each element in the BNNLayer
mutateBNN16 :: RandomGen g => BNN16 -> Rand g BNN16
mutateBNN16 bnn = V.forM bnn mutateBNNLayer

-- Apply function "mutateBNN16" to each element in the BNP
mutateBNP16 :: RandomGen g => BNP16 -> Rand g BNP16
mutateBNP16 bnp = V.forM bnp mutateBNN16

-- Given a full population, it will take a subset with "size" length, then
-- apply mutation to each BNN16
mutateBNP16Size :: RandomGen g => BNP16 -> Int -> Rand g BNP16
mutateBNP16Size bnp size = mutateBNP16 toMutate
    where toMutate = V.take size bnp
