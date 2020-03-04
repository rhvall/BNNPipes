----
---- BNN-Pipes, Copyright (C) 21/Sep/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

module BNN.BNU
-- (
--
-- )
where

import           BNN.VectorUtilities
import           Data.Bits           (complement, complementBit, popCount,
                                      testBit, xor, (.&.), (.|.))
import           Data.Bool           (bool)
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector         as V
import           Data.Word           (Word8)
import           Foreign.Storable    (sizeOf)
import           ListExtras          (applyNTimes)
-- The basic binary type on which BNU operates
type BNUBType = Word8

bnuBSize :: BNUBType
bnuBSize = 8

bnuBSizeInt :: Int
bnuBSizeInt = fromIntegral bnuBSize

-- Generic data handler
type BNUVType = V.Vector BNUBType
-- Vector Threshold Update container
type DeltaChangeV = V.Vector DeltaChange

data BNUUpdateType = Weight | Threshold
    deriving (Show, Eq)

data DeltaChange = RemainT | IncreaseT | DecreaseT
    deriving (Show, Eq)

data BNU = BNU {
    bnuW :: BNUVType,
    bnuT :: BNUVType
} deriving (Show, Eq)

data BNUUpdate = BNUUpdate {
    bnuUT    :: BNUUpdateType,
    bnuWLast :: BNUBType,
    bnuTLast :: BNUBType
} deriving (Show, Eq)

data BNUComp = BNUComp {
    bnuWT     :: BNU,
    bnuUpdate :: BNUUpdate,
    bnuXOR    :: BNUVType
} deriving (Show, Eq)

data QueryElem = QueryElem {
    inQ  :: BNUBType,
    outQ :: BNUBType
} deriving (Eq, Show)

queryBNU :: BNU -> BNUBType -> BNUBType
queryBNU bnuOp@(BNU _ thresholds) input = f $ xorBNU bnuOp input
    where f = bool2BNUBType . V.zipWith (\x y -> x <= fromIntegral (popCount y)) thresholds

xorBNU :: BNU -> BNUBType -> BNUVType
xorBNU (BNU weights _) input = xorRes
  where xorRes = V.map (`xor` input) weights

bool2BNUBType :: V.Vector Bool -> BNUBType
bool2BNUBType = V.ifoldl' (\acc idx val -> bool acc (acc .|. fromIntegral (2 ^ idx)) val) 0
    -- where vecR = V.reverse vec
--bool2BNUBType = V.ifoldr' (\idx val acc -> bool acc (acc .|. fromIntegral (2 ^ idx)) val) 0

deltaWeight :: BNUVType -> BNUBType -> BNUBType -> BNUVType
deltaWeight xorWeights out delta = V.zipWith3 (\x y z -> x .&. xor y z) deltaVec nout xorWeights
    where nout = constructVector $ complement out
          deltaVec = constructVector delta

deltaThreshold :: BNUVType -> BNUBType -> BNUBType -> BNUVType
deltaThreshold thresholds out delta = V.imap applyT thresholds
    where boolD idx = bool 0 (bool (-1) 1 (testBit out idx)) (testBit delta idx)
          applyT idx x = let toMod = boolD idx in bool (x + toMod) x (x == minBound && (x + toMod == maxBound))

deltaThreshold' :: BNUBType -> BNUBType -> DeltaChangeV
deltaThreshold' out delta = V.generate (sizeOf bnuBSize * 8) boolD
    where boolD idx = bool RemainT (bool DecreaseT IncreaseT (testBit out idx)) (testBit delta idx)

applyDeltaWeight :: BNUVType -> BNUVType -> BNUBType -> (BNUVType, BNUBType)
applyDeltaWeight deltaW weights lastWU = (mapped, nextWU)
    where oredDelta = foldr (.|.) 0 deltaW
          nextWU = fromMaybe 0 $ findNextIndexL oredDelta (lastWU + 1) 0
          nextWUI = fromIntegral nextWU
          mapped = V.zipWith (\x y -> bool x (complementBit x nextWUI) (testBit y nextWUI) ) weights deltaW

applyDeltaThreshold :: DeltaChangeV -> BNUVType -> BNUBType -> (BNUVType, BNUBType)
applyDeltaThreshold deltaT thresholds lastT = evaluateElement element thresholds lastT'
    where zipped = V.izipWith (\idx x y -> (idx, x, y)) deltaT thresholds
          lastT' = bool (lastT + 1) 0 (lastT + 1 >= bnuBSize)
          moved = applyNTimes (fromIntegral lastT') shiftLeftV zipped
          element = V.find (\(_, x, _) -> x /= RemainT) moved

evaluateElement :: Maybe (Int, DeltaChange, BNUBType) -> BNUVType -> BNUBType -> (BNUVType, BNUBType)
evaluateElement Nothing thresholds lastT = (thresholds, lastT)
evaluateElement (Just (idx, deltaC, value)) thresholds _ = (mapped, deltaIdx)
    where updateVal = updateValue value deltaC
          mapped = thresholds V.// [(idx, updateVal)]
          deltaIdx = fromIntegral idx

updateBNU :: BNU -> BNUUpdate -> BNUBType -> BNUBType -> BNUBType -> (BNU, BNUUpdate)
updateBNU (BNU weights thresholds) (BNUUpdate update lastW lastT) input output delta
    | delta <= 0 = (BNU weights thresholds, BNUUpdate update lastW lastT)
    | update == Weight = (BNU weights deltaTU, BNUUpdate Threshold lastW lastUT )
    | otherwise = (BNU deltaWU thresholds, BNUUpdate Weight lastUW lastT)
    where xorWeights = V.map (xor input) weights
          deltaW = deltaWeight xorWeights output delta
          (deltaWU, lastUW) = applyDeltaWeight deltaW weights lastW
          deltaT = deltaThreshold' output delta
          (deltaTU, lastUT) = applyDeltaThreshold deltaT thresholds lastT

updateValue :: BNUBType -> DeltaChange -> BNUBType
updateValue x RemainT   = x
updateValue x IncreaseT
    | x == maxBound = x
    | otherwise = x + 1
updateValue x DecreaseT
    | x == minBound = x
    | otherwise = x - 1

updateTypeCounter :: Int -> (Int, BNUUpdateType) -> (Int, BNUUpdateType)
updateTypeCounter maxCount (idxLast, Weight) = (bool (idxLast + 1) 1 (idxLast > maxCount), Threshold)
updateTypeCounter _ (idxLast, Threshold) = (idxLast, Weight)


--------------------------------------------------------------------------------
----------------------------- Mutation Operations ------------------------------
--------------------------------------------------------------------------------

applyBitOpr :: (BNUBType -> BNUBType -> BNUBType) -> (BNUBType -> BNUBType -> BNUBType)
                -> BNU -> BNU -> BNU
applyBitOpr bitOprW bitOprT (BNU w0 t0) (BNU w1 t1)  = BNU w2 t2
    where w2 = V.zipWith bitOprW w0 w1
          t2 = V.zipWith (checkBounds bitOprT) t0 t1
          checkBounds f x y = bool (f x y) x (f x y >= bnuBSize)

andBNUOpr :: BNU -> BNU -> BNU
andBNUOpr = applyBitOpr (.&.) (+)

orBNUOpr :: BNU -> BNU -> BNU
orBNUOpr =  applyBitOpr (.|.) (-)

xorBNUOpr :: BNU -> BNU -> BNU
xorBNUOpr = applyBitOpr xor (+)

xnorBNUOpr :: BNU -> BNU -> BNU
xnorBNUOpr =  applyBitOpr (\x y -> complement $ xor x y) (-)

