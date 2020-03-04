----
---- BNN-Pipes, Copyright (C) 29/Dec/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE TemplateHaskell #-}

module BNN.BNU16Tests
-- (
--
-- )
where

import           BNN.BNU16
import           Data.Bits                (popCount, xor, (.&.), (.|.))
import           Data.Bool                (bool)
import qualified Data.Vector              as V
import           QuickCheck.ArbitraryDefs ()
import           Test.Tasty               (TestTree)
import           Test.Tasty.QuickCheck    (testProperty)
import           Test.Tasty.TH            (testGroupGenerator)

prop_bnuOprAnd :: BNU16Type -> BNU16Type -> Bool
prop_bnuOprAnd x y = bnuOpr (.&.) x y == z
    where (x', y', t) = helperWeightThreshold x y
          z = popCount (x' .&. y') >= t

prop_bnuOprOr :: BNU16Type -> BNU16Type -> Bool
prop_bnuOprOr x y = bnuOpr (.|.) x y == z
    where (x', y', t) = helperWeightThreshold x y
          z = popCount (x' .|. y') >= t

prop_bnuOprXOR :: BNU16Type -> BNU16Type -> Bool
prop_bnuOprXOR x y = bnuOpr xor x y == z
    where (x', y', t) = helperWeightThreshold x y
          z = popCount (xor x'  y') >= t

prop_bnuVOprAnd :: BNU16Type -> BNU16 -> Bool
prop_bnuVOprAnd x y = bnuVOpr (.&.) x y == fld
    where bnuOprs = V.map (bnuOpr (.&.) x) y
          fld = V.ifoldr (\idx w z -> bool z ((2 ^ idx) .|. z) w) 0 bnuOprs

prop_bnuVOprOr :: BNU16Type -> BNU16 -> Bool
prop_bnuVOprOr x y = bnuVOpr (.|.) x y == fld
    where bnuOprs = V.map (bnuOpr (.|.) x) y
          fld = V.ifoldr (\idx w z -> bool z ((2 ^ idx) .|. z) w) 0 bnuOprs

prop_bnuVOprXOR :: BNU16Type -> BNU16 -> Bool
prop_bnuVOprXOR x y = bnuVOpr xor x y == fld
    where bnuOprs = V.map (bnuOpr xor x) y
          fld = V.ifoldr (\idx w z -> bool z ((2 ^ idx) .|. z) w) 0 bnuOprs

tests :: TestTree
tests = $(testGroupGenerator)

--------------------------------------------------------------------------------
--------------------------Helper Methods----------------------------------------
--------------------------------------------------------------------------------

helperWeightThreshold :: BNU16Type -> BNU16Type -> (BNU16Type, BNU16Type, Int)
helperWeightThreshold x y = (weightBits x, weightBits y, thresholdBitsI y)
