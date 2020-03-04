----
---- BNN-Pipes, Copyright (C) 30/Dec/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

module QuickCheck.ArbitraryDefs
-- (
--
-- )
where

import           BNN.BNN16             (Query16 (..), QueryVec (..))
import qualified Data.Vector           as V
import           System.Random         (StdGen, mkStdGen)
import           Test.Tasty.QuickCheck (Arbitrary, arbitrary)

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary

instance Arbitrary Query16 where
    arbitrary = Query16 <$> arbitrary <*> arbitrary

instance Arbitrary QueryVec where
    arbitrary = QueryVec <$> arbitrary <*> arbitrary

instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary
