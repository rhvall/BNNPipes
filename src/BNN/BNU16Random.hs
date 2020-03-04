----
---- BNN-Pipes, Copyright (C) 23/Dec/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE BangPatterns #-}

-- Module that defines the helper methods to initialize BNU16 from random
-- values
module BNN.BNU16Random
-- (
--
-- )
where

import           BNN.BNN16                   (BNN16, BNNLayer)
import           BNN.BNP16                   (BNP16)
import           BNN.BNU16                   (BNU16, BNU16Type, maxThreshold,
                                              maxWeight, shiftBackTr)
import           Control.Monad.Random.Strict (Rand, RandomGen, getRandomR)
import           Data.Bits                   (popCount, setBit, shiftL, shiftR,
                                              xor, (.&.), (.|.))
import           Data.Bool                   (bool)
import qualified Data.Vector                 as V
import           Data.Word                   (Word16)

-- Generate a random weight value, it considers the maximum bound
-- given a limit of "maxWeight" 12 bits (4095)
randomWeight :: RandomGen g => Rand g BNU16Type
randomWeight = getRandomR (0, maxWeight)

-- Generate a random threshold value, it considers the maximum bound
-- given a limit of 12 in comparison to the weight's bits. Also, it
-- moves those enabled bits to the first nibble in the BNU16Type
randomThreshold :: RandomGen g => Rand g BNU16Type
randomThreshold = shiftBackTr <$> getRandomR (0, maxThreshold)

-- Form a random binary element that has a 12 bit weight and a 4 bit
-- thresdhold in it. Returns as well the updated seed
randomBinElem :: RandomGen g => Rand g BNU16Type
randomBinElem = do
    !rw <- randomWeight
    !rt <- randomThreshold
    return $ rw .|. rt

-- Generate a random BNUV16 vector with the size indicated in the
-- second parameter
randomBNU16 :: RandomGen g => Int -> Rand g BNU16
randomBNU16 sizeBNU = V.replicateM sizeBNU randomBinElem

-- Generate a random BNUV16 vector with the size indicated in the
-- second parameter
randomBNNLayer :: RandomGen g => Int -> Int -> Rand g BNNLayer
randomBNNLayer sizeBNU sizeLayer = V.generateM sizeLayer (const f)
    where f = randomBNU16 sizeBNU

-- Generate random BNN16 given a size of elements inside the BNU and
-- a number of units per binary network
randomBNN16 :: RandomGen g => Int -> Int -> Int -> Rand g BNN16
randomBNN16 sizeBNU sizeLayer sizeNet = V.generateM sizeNet (const f)
    where f = randomBNNLayer sizeBNU sizeLayer

-- Generate random BNP16 given a size of elements inside the BNU16, BNN16 and
-- a number of networks needed to create a population
randomBNP16 :: RandomGen g => Int -> Int -> Int -> Int -> Rand g BNP16
randomBNP16 sizeBNU sizeLayer sizeNet sizePop = V.generateM sizePop (const f)
    where f = randomBNN16 sizeBNU sizeLayer sizeNet

-- Generate a random BNU16 vector with up to the size indicated in the
-- sizeBNU parameter, with the difference that it will be of irregular
-- size, randomly chosen as well, guaranteed to have at least one element
irrRandomBNU16 :: RandomGen g => Int -> Rand g BNU16
irrRandomBNU16 sizeBNU = getRandomR (1, sizeBNU) >>= f
    where f x = V.replicateM x randomBinElem

-- Generate a random BNNLayer with up to the size indicated in the
-- sizeLayer parameter, with the difference that it will be of irregular
-- size, randomly chosen as well, guaranteed to have at least one element
irrRandomBNNLayer :: RandomGen g => Int -> Int -> Rand g BNNLayer
irrRandomBNNLayer sizeBNU sizeLayer = getRandomR (1, sizeLayer) >>= f
    where f x = V.replicateM x (irrRandomBNU16 sizeBNU)

-- Generate a random BNN16 with up to the size indicated in the
-- sizeTree parameter, with the difference that it will be of irregular
-- size, randomly chosen as well, guaranteed to have at least one element
irrRandomBNN16 :: RandomGen g => Int -> Int -> Int -> Rand g BNN16
irrRandomBNN16 sizeBNU sizeLayer sizeTree = getRandomR (1, sizeTree) >>= f
    where f x = V.replicateM x (irrRandomBNNLayer sizeBNU sizeLayer)
