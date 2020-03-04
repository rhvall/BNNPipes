----
---- BNN-Pipes, Copyright (C) 23/Dec/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- Module that defines a vector of BNU16, it can be queried with data types
-- of "BNU16Type", it ensembles on top of the BNU16 module. BNN16 stands for
-- Binary Neural Network and 16 represents the data length
module BNN.BNN16
-- (
--
-- )
where

import           BNN.BNU16           (BNU16, BNU16Type, boolToBNU16Type,
                                      queryBNU16, queryBNU16V, weight16SizeI)
import           BNN.VectorUtilities (splitChunks)
import           Data.Bits           (xor)
import           Data.Bool           (bool)
import qualified Data.Vector         as V

-- Synoninm to represent a layer of BNU16 to be renamed as BNNLayer
type BNNLayer = V.Vector BNU16

-- Synoninm to represent a BNN that has many BNNLayers
type BNN16 = V.Vector BNNLayer

-- Synoninm to compact vector of BNU16Tyoe considered as BNNOutput
type BNNOutput = V.Vector BNU16Type

-- Synoninm to compact vector of BNU16Tyoe considered as BNPIntput
type BNNInput = V.Vector BNU16Type

-- Structure to contain input values along with expected outputs
-- with BNU16Type format
data Query16 = Query16 {
    inQ  :: BNU16Type,
    outQ :: BNU16Type
} deriving (Eq, Show)

data QueryVec = QueryVec {
    inQV  :: BNNInput,
    outQV :: BNNOutput
} deriving (Eq, Show)

-- Given a BNNInput and a BNNLayer, it outputs the value of quering
-- each of their binary units.  Example:
-- input = [1, 2]
-- BNNLayer = [[48680,10697],[50831,1607]]
-- Output: [10]
queryBNNLayer :: BNNInput -> BNNLayer -> BNNOutput
queryBNNLayer vecIn layer = V.map boolToBNU16Type splitted
    where zipped = V.zipWith queryBNU16V vecIn layer
          joined = V.foldl' (V.++) V.empty zipped
          splitted = V.convert $ splitChunks joined weight16SizeI

-- Given a BNNInput and a BNN16, it outputs the value
-- of quering each of their binary layers.  Example:
-- input = [1, 2]
-- BNN = [[[48680,10697],[50831,1607]],[[25305,48277],[49011,46793]]]
-- Output: [1]
queryBNN16 :: BNNInput -> BNN16 -> BNNOutput
queryBNN16 = V.foldl' queryBNNLayer

-- Simpler function to calculate the hamming distance between the
-- expected value and the output of the BNN. Example:
-- QueryVec [9] [81] (input / output query elements)
-- BNN = [[[48680,10697],[50831,1607]],[[25305,48277],[49011,46793]]]
-- Output: 80 (0101 0000)
distanceBNN16 :: QueryVec -> BNN16 -> BNNOutput
distanceBNN16 (QueryVec inQV outQV) bnn = V.zipWith xor query outQV
    where query = queryBNN16 inQV bnn

-- Calculates the hamming distance between the expected value and the
-- output of the BNN considering that input is replicated to each BNU
-- in the layer and the output takes the lowest distance between the
-- expectation and BNN16 outputs. Dist stands for distributed.
distanceBNN16Dist :: Query16 -> BNN16 -> BNU16Type
distanceBNN16Dist (Query16 inQE outQE) bnn = undefined -- xor outQE $ queryBNN16 inQE bnn

