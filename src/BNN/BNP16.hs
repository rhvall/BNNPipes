----
---- BNN-Pipes, Copyright (C) 24/Dec/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- Module that defines a vector of BNP16, otherwise known as a "population".
-- It contains functions that help query vectors of BPN16. BNP16 stands for
-- Binary Neural Population and 16 represents the data length. This is the
-- schematics of hierarchies:
-- BNP16 --- contains ---> BNN16 --- contains ---> BNU16
module BNN.BNP16
-- (
--
-- )
where

import           BNN.BNN16   (BNN16, BNNInput, BNNOutput, Query16, QueryVec,
                              distanceBNN16, queryBNN16)
import           BNN.BNU16   (BNU16Type)
import           Data.Bits   (xor)
import qualified Data.Vector as V

-- Synoninm to compact vectors of BNN16 to be renamed as BNP16
type BNP16 = V.Vector BNN16

-- Synoninm to compact vector of Query16 considered as BNPQuery
type BNPQuery = V.Vector Query16

-- Synoninm to compact vector of QueryVec considered as BNPQueryVec
type BNPQueryVec = V.Vector QueryVec

-- Synoninm to compact vector of BNNOutput considered as BNPOutput.
-- This simplifies that each BNN16 in a population has an inference
-- result, that later should be considered by the evolutionary algorithm
type BNPOutput = V.Vector BNNOutput

-- Given a BNNInput and a BNP, it outputs the value of quering each
-- of their binary neural networks. Its result provides a vector
-- where each component is the result of asking the population
-- which is their inference value
queryBNP16 :: BNNInput -> BNP16 -> BNPOutput
queryBNP16 input = V.map (queryBNN16 input)

-- Forward function to calculate the hamming distance between the
-- expected value and the output of the BNP.
distanceBNP16 :: QueryVec -> BNP16 -> BNPOutput
distanceBNP16 input = V.map (distanceBNN16 input)
