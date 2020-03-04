----
---- BNN-Pipes, Copyright (C) 11/Nov/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

module BNN.BNP
-- (
--
-- )
where

import           BNN.BNU
import           Data.Bits
import qualified Data.Vector as V

type BNP = V.Vector BNU
type BNPBatch = V.Vector BNP

queryBNP :: BNP -> BNUBType -> BNUBType
queryBNP bnp input = V.foldl (flip queryBNU) input bnp

distanceBNP :: BNP -> QueryElem -> BNUBType
distanceBNP bnp (QueryElem inQE outQE) = countDiff outQE $ queryBNP bnp inQE
    where countDiff x y = fromIntegral $ popCount (x `xor` y)

andBNPOpr :: BNP -> BNP -> BNP
andBNPOpr = V.zipWith andBNUOpr

orBNPOpr :: BNP -> BNP -> BNP
orBNPOpr = V.zipWith orBNUOpr

xorBNPOpr :: BNP -> BNP -> BNP
xorBNPOpr = V.zipWith xorBNUOpr

xnorBNPOpr :: BNP -> BNP -> BNP
xnorBNPOpr = V.zipWith xnorBNUOpr
