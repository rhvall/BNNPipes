----
---- BNN-Pipes, Copyright (C) 31/Dec/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

module Evol.SelectBNP
-- (
--
-- )
where

import           BNN.BNN16            (BNN16, BNNLayer)
import           BNN.BNP16            (BNP16)
import           BNN.BNU16            (BNU16Type)
import           BNN.BNUEvo           (mutateBType)
import           Control.Monad        (replicateM)
import           Control.Monad.Random.Strict (Rand, RandomGen, getRandomR)
import           Data.Bool            (bool)
import           Data.Maybe           (Maybe, catMaybes, fromJust, isJust)
import qualified Data.Vector          as V

-- This constant will count the number of tries that a recursive function
-- should be called, if it exceeds, it will fail.
maxNumTries :: BNU16Type
maxNumTries = 5

-- This function will take a BNN and the size of the desired population
-- to be created. It will select at random the index that will be mutated,
-- but only one change at the time will occur. Example:
-- BNN = [[22792,35203],[24628,47101]]
-- size = 2
-- BNP = [[[49548,35203],[24628,47101]],[[22792,1744],[24628,47101]]]
-- As can be infered from the example, BNNs inside BNP are relatively similar
-- to the original BNN, with indexes (0,0) & (0,1) changed
createPopByMut :: RandomGen g => BNN16 -> Int -> Rand g BNP16
createPopByMut bnn size
    | size <= 0 = return V.empty
    | V.null bnn = return V.empty
    | otherwise = rdm >>= \x -> V.forM x (updateBNN bnn)
        where rdm = rdmIdxes bnn size


-- With a BNNLayer and a given size, it will generate a new vector that has a
-- valid pair that indexes values within the BNNLayer argument, it will
-- have as much elements (or less) as the provided size, considering
-- that in some cases (due to randomness) the index size will be
-- less than the value provided, specially if the BNN16 is small. In
-- case "bnl" is empty, it will return an empty vector. Example:
-- BNN16 = [[[17410,7416],[43404,10355]],[[1090,42230],[11719,15783]]]
-- size = 4
-- res = [(1,1),(1,0),(2,1),(0,1)]
-- TODO!! Remove duplicates
rdmIdxes :: RandomGen g => BNN16 -> Int -> Rand g (V.Vector (Int, Int, Int))
rdmIdxes bnn size
    | V.null bnn = return V.empty
    | size <= 0 = return V.empty
    | otherwise = do
        rdm <- V.generateM size (\_ -> rdmIdx bnn maxNumTries)
        return . V.map fromJust $ V.filter isJust rdm

-- Helper method to obtain a valid index from a BNNLayer and a counter of the
-- maximum number of tries if the random selection does not fit a correct
-- index. If after that many attempts does not work, it will dismiss with
-- a Nothing value
rdmIdx :: RandomGen g => BNN16 -> BNU16Type -> Rand g (Maybe (Int, Int, Int))
rdmIdx bnn maxTries
    | maxTries <= 0 = return Nothing
    | V.null bnn = return Nothing
    | otherwise = do
        xM <- validIndex bnn maxTries
        case xM of
            Nothing -> return Nothing
            Just x -> do
                yM <- validIndex (bnn V.! x) maxTries
                case yM of
                    Nothing -> return Nothing
                    Just y -> do
                        z <- fRand $ V.length (bnn V.! x V.! y)
                        return $ Just (x,y,z)

-- This function updates a BNNLayer given a set of coordinates (x, y). It
-- should receive a valid location within it, otherwise it will return
-- an empty vector. Example:
-- BNNLayer = [[16270,5286],[7567,35551]]
-- x = 1
-- y = 0
-- result = [[16270,5286],[38736,35551]]
-- Location (1,0) was changed from 7567 to 38736.
updateBNN :: RandomGen g => BNN16 -> (Int, Int, Int) -> Rand g BNN16
updateBNN bnn (x, y, z)
    | V.null bnn = return V.empty
    | x < 0 || y < 0 || z < 0 = return V.empty
    | x >= V.length bnn = return V.empty
    | y >= V.length (bnn V.! x) = return V.empty
    | otherwise = do
        let (bnl, bnu, bnt) = (bnn V.! x, bnl V.! y, bnu V.! z)
        bnt' <- mutateBType bnt
        let bnu' = bnu V.// [(z, bnt')]
        let bnl' = bnl V.// [(y, bnu')]
        return $ bnn V.// [(x, bnl')]

--------------------------------------------------------------------------------
--------------------------Helper Methods----------------------------------------
--------------------------------------------------------------------------------

-- Helper method to obtain a valid index from a vector of vectors, using
-- counter for the maximum number of tries if the random variable does
-- not fit a correct index. If after that many attempts does not work,
-- it will dismiss with a Nothing value
validIndex :: RandomGen g => V.Vector (V.Vector a) -> BNU16Type -> Rand g (Maybe Int)
validIndex vec maxTries
    | maxTries <= 0 = return Nothing
    | V.null vec = return Nothing
    | otherwise = do
        xVal <- fRand $ V.length vec
        let selection = V.length $ vec V.! xVal
        bool (validIndex vec (maxTries - 1)) (return $ Just xVal) (selection > 0)

-- Simple helper to get a random value in the interval 0, x - 1
fRand :: RandomGen g => Int -> Rand g Int
fRand x = getRandomR (0, x - 1)
