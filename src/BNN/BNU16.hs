----
---- BNN-Pipes, Copyright (C) 23/Dec/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- Module that defines the simplest unit in a BNN, it handles data types of
-- "Word16" that encodes weights and thresholds within the same value, given
-- that 12 bits are used for weights and 4 bits are assigned to thresholds.
-- BNU16 stands for Binary Neuro Unit and 16 represents the data length
module BNN.BNU16
-- (
--
-- )
where

import           Data.Bits   (popCount, setBit, shiftL, shiftR, xor, (.&.),
                              (.|.))
import           Data.Bool   (bool)
import qualified Data.Vector as V
import           Data.Word   (Word16)

-- Limit values and their binary representations
-- 15    (0000 0000 0000 1111)
-- 4095  (0000 1111 1111 1111)
-- 65535 (1111 1111 1111 1111)
-- 65520 (1111 1111 1111 0000)
-- 61440 (1111 0000 0000 0000)

-- The basic binary type on which BNU operates
type BNU16Type = Word16

-- Synoninm to represent a binary neural unit of 16 bits,
-- where 12 are weights and 4 are thresholds.
type BNU16 = V.Vector BNU16Type

-- The number of bits a weight uses in the 16 bit word
weight16Size :: BNU16Type
weight16Size = 12

-- Synoninm to Integer value
weight16SizeI :: Int
weight16SizeI = fromIntegral weight16Size

-- The number of bits a threshold uses in the 16 bit word
threshold16Size :: BNU16Type
threshold16Size = 4

-- Maximum value that a weight can reach
maxWeight :: BNU16Type
maxWeight = 4095

-- Synoninm to Integer value
maxWeightI :: Int
maxWeightI = fromIntegral maxWeight

-- Maximum value that a threshold can reach
maxThreshold :: BNU16Type
maxThreshold = 12

-- Synoninm to Integer value
maxThresholdI :: Int
maxThresholdI = fromIntegral maxThreshold

-- Highest function level that takes input as BNU16Type and
-- returns the response from the binary unit
queryBNU16 :: BNU16Type -> BNU16 -> BNU16Type
queryBNU16 = bnuVOpr xor

-- Highest function level that takes input as BNU16Type and
-- returns the response from each binary unit as they are
-- queried using the xor function
queryBNU16V :: BNU16Type -> BNU16 -> V.Vector Bool
queryBNU16V = bnuMapped xor

-- Basic BNU operation of weights and thresholds, taking a
-- input in BNU16Type  form and outputing boolean if the
-- result of the applied "binFunc" is successful or not. As
-- example with the "xor" function:
-- oprWT = 8230     (0010 0000 0010 0110)
-- input = 34       (0000 0000 0010 0010)
-- weight = 38      (0000 0000 0010 0110) Only the first 12 bits
-- --------------------------------------
-- weighted = 4      (0000 0000 0000 0100)
-- popWeight = 1     Only one bit is set in weighted
-- threshold = 2    (0000 0000 0000 0010) Only the last 4 bits shifted
-- result = 0       popWeight is not bigger than threshold
bnuOpr :: (BNU16Type -> BNU16Type -> BNU16Type) -> BNU16Type -> BNU16Type -> Bool
bnuOpr binFunc input oprWT = popWeight >= threshold
    where weighted = uncurry binFunc (weightBits oprWT, weightBits input)
          popWeight = popCount weighted
          threshold = thresholdBitsI oprWT

-- Helper function to map a binary function with an input to each element
-- of the BNU16 obtaining their boolean responses. Example:
-- binFunc = .|.
-- input = 4
-- bnu = [20308,50923,33929,43616]
-- result = [True,False,False,False]
bnuMapped :: (BNU16Type -> BNU16Type -> BNU16Type) -> BNU16Type -> BNU16 -> V.Vector Bool
bnuMapped binFunc input = V.map (bnuOpr binFunc input)

-- Basic BNU operation on vector of weights and thresholds; given
-- a binary function, an input, it returns the query value to that
-- BNU. An example follows:
-- binFunc = xor
-- input = 1036 (0000 0100 0000 1100)
-- vecBNU = [25156, 17236, 4375]
-- boolVec = [False, True, True]
-- boolVecIndex = [0, 1, 2]
-- result = 6  (0000 0000 0000 0110)
bnuVOpr :: (BNU16Type -> BNU16Type -> BNU16Type) -> BNU16Type -> BNU16 -> BNU16Type
bnuVOpr binFunc input = boolToBNU16Type . bnuMapped binFunc input

-- Wrapper to make it an integer
thresholdBitsI :: BNU16Type -> Int
thresholdBitsI = fromIntegral . thresholdBits

-- Transform a boolean vector to a BNU16Type, considering the maximum value
-- of "maxWeight" which is the limut a BNU can take in their weight part. Example:
-- vec = [False,True,True,False,True,True,False,True,True,False,True,True]
-- result = 3510 (0000 1101 1011 0110)
boolToBNU16Type :: V.Vector Bool -> BNU16Type
boolToBNU16Type vec
    | V.length vec > maxWeightI = 0
    | otherwise = V.ifoldl' boolTo16 0 vec
    where boolTo16 y idx = bool y (setBit y idx)

--------------------------------------------------------------------------------
-----------------------------Bit Manipulation-----------------------------------
--------------------------------------------------------------------------------

-- Use only the first 12 bits of Word16 to represent weights, the first
-- bits will be set to 0. Example:
-- input = 8226     (0010 0000 0010 0010)
-- result = 36      (0000 0000 0010 0010)
weightBits :: BNU16Type -> BNU16Type
weightBits = (.&.) maxWeight

-- Use only the first 4 bits of the Word16 to represen thresholds,
-- everything else is set to 0 and moved to the right. Example:
-- input = 8226     (0010 0000 0010 0010)
-- result = 36      (0000 0000 0000 0010)
thresholdBits :: BNU16Type -> BNU16Type
thresholdBits = fromIntegral . flip shiftR maxThresholdI . (.&.) 61440

-- Move the first nibble to the last position. Example:
-- input = 8      (0000 0000 0000 0100)
-- output = 32768 (0100 0000 0000 0000)
shiftBackTr :: BNU16Type -> BNU16Type
shiftBackTr = flip shiftL maxThresholdI . (.&.) 15

-- Take two BNU16Type to create a single element that uses all bits
-- considering that neither fulfills the 12 bits of maximum output a
-- BNU16 could provide, modulated by the position parameter that dictates
-- where input2 will start, in other words, the number of bits to be shifted
-- left which are ocuppied by input2. Example
-- input1 = 5   (0000 0000 0000 0101)
-- input2 = 9   (0000 0000 0000 1001)
-- pos = 3
-- output = 77  (0000 0000 0100 1101)
mergeBNU16Type :: BNU16Type -> BNU16Type -> Int -> BNU16Type
mergeBNU16Type in1 in2 pos
    | pos < 0 = 0
    | pos >= weight16SizeI = in1
    | otherwise = movedIn2 .|. clearBits in1 pos
    where movedIn2 = shiftL in2 pos

-- From an input, set to 0 all those after the position parameter. Example:
-- input = 237  (1110 1101)
-- pos = 5
-- output = 13  (0000 1101)
clearBits :: BNU16Type -> Int -> BNU16Type
clearBits input pos = input .&. (2 ^ pos - 1)
