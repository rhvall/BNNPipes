----
---- BNN-Pipes, Copyright (C) 21/Sep/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE FlexibleContexts #-}

module BNN.VectorUtilities
-- (
--
-- )
where

import           Data.Bits                    (rotate, shiftL, testBit, (.&.))
import           Data.Bool
import qualified Data.Set                     as S
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Merge as VM
import qualified Data.Vector.Unboxed          as VU
import           Data.Word                    (Word8)

-- High order function that folds over a vector of values and seeds
foldGeneric :: (b -> p -> (a, b)) -> b -> V.Vector p -> (V.Vector a, b)
foldGeneric m s = V.foldl f (V.empty, s)
    where f (x, g) y = let (k, g') = m g y in (V.cons k x, g')

-- High order function that folds over a vector of values and seeds using
-- monadic values
foldGenericM :: Monad m => (b -> p -> m (a, b)) -> b -> V.Vector p -> m (V.Vector a, b)
foldGenericM t s vec = do
    let f (x, g) y = t g y >>= \(k, g') -> return (V.cons k x, g')
    V.foldM f (V.empty, s) vec

-- Receive a vector of arbitrary elements and a vector of indexes that
-- will pair those elements to. Have in consideration that "idxs" should
-- provide a valid indexes that are between 0 and "vec" size, otherwise
-- this function will break; also, "idx" will pair up to "vec" size,
-- starting from 0, anything else will be discarded. Example:
-- vec = [3, 5, 7, 9, 12, 4]
-- idxs = [2, 4, 5, 6]
-- result = [(3,7), (5, 4), (7, 12), (9, 4)]
-- In this example, 12 and 4 were not used by the function becasue
-- "idxs" was smaller than "vec" size
pairElements :: V.Vector p -> V.Vector Int -> V.Vector (p,p)
pairElements vec idxs
    | V.null vec = V.empty
    | V.null idxs = V.empty
    | otherwise = V.zipWith (\x y -> (x, vec V.! y)) vec idxs

-- Sort a vector using another vector that simbilizes a scored obtained by the
-- first given a certain metric, it will always place values in increasing order
sortUsingVec :: (Ord a) => V.Vector p -> V.Vector a -> V.Vector p
sortUsingVec vec scores = V.map fst sorted
    where zipped = V.zip vec scores
          sorted = V.modify (VM.sortBy (\(_, x) (_, y) -> compare x y)) zipped

-- With an arbitrary vector and a positive integer of known size it will create
-- a vector that has splitted the original into sections. Example:
-- vec = [1,2,3,4,5,6,7,8,9,10]
-- size = 4
-- result = [[1,2,3,4],[5,6,7,8],[9,10]]
splitChunks :: V.Vector p -> Int -> V.Vector (V.Vector p)
splitChunks vec size
    | size <= 0 = V.empty
    | otherwise = fst $ until (V.null . snd) opr (V.empty, vec)
    where opr (x, y) = let (l, r) = V.splitAt size y in (V.snoc x l, r)

-- instance Ord StdGen where
--     compare g g' = compare (show g) (show g')
--
-- instance Eq StdGen where
--     g == g' = show g == show g'

-- Remove elements that are duplicated in the vector using a helper Set. It
-- should do it in one pass with O(n) cost, however, it transforms from Set
-- to List then to Vector back again, which should be O(3n) -> O(n)
removeDups :: (Ord a) => V.Vector a -> V.Vector a
removeDups vec
    | V.null vec = V.empty
    | otherwise = V.fromList $ S.toList folded
    where folded = V.foldr S.insert S.empty vec

-- Given a vector of elements, it will pair them reducing its size by half,
-- dicarding odd elements at the end. Example:
-- vec = [1,2,3,4,5]
-- result = [(1,2), (3,4)]
pairVec :: V.Vector p -> V.Vector (p, p)
pairVec vec
    | V.null vec = V.empty
    | otherwise = fst carried
    where fstElem = V.head vec
          carried = V.ifoldl (\(v, c) idx y -> bool (addV v c y, c) (v, y) (mod idx 2 == 0)) (V.empty, fstElem) vec
          addV v x y = V.snoc v (x, y)

splitAtEveryLength :: Int -> V.Vector (V.Vector a) -> V.Vector a -> V.Vector (V.Vector a)
splitAtEveryLength vecSize acc vec
    | V.null vec = acc
    | otherwise = splitAtEveryLength vecSize (V.snoc acc fs) sn
    where (fs, sn) = V.splitAt vecSize vec

unitMatrix :: V.Vector Word8
unitMatrix = V.iterateN 8 (`shiftL` 1) 1

shiftLeftV :: V.Vector a -> V.Vector a
shiftLeftV x
    | V.null x = V.empty
    | otherwise = V.snoc (V.tail x) (V.head x)

constructVector :: Word8 -> V.Vector Word8
constructVector word = V.generate 8 (bool 0 255 . testBit word)

shiftRightV :: V.Vector a -> V.Vector a
shiftRightV x
    | V.null x = V.empty
    | otherwise = V.cons (V.last x) (V.init x)

findNextBitL :: Word8 -> Word8 -> Word8
findNextBitL 0 _ = 0
findNextBitL _ 0 = 0
findNextBitL x y = until (\z -> (x .&. z) /= 0) (`rotate` 1) y

findNextIndexL :: Word8 -> Word8 -> Word8 -> Maybe Word8
findNextIndexL 0 _ _ = Nothing
findNextIndexL _ _ 7 = Nothing
findNextIndexL target indexer counter
    | counter < 0 = Nothing
    | indexer < 0 = Nothing
    | otherwise = bool (findNextIndexL target indexer' counter') (Just indexer) (testBit target (fromIntegral indexer))
    where indexer' = bool (indexer + 1) 0 (indexer + 1 >= 8)
          counter' = counter + 1

findNextBitLIndex :: Word8 -> Int
findNextBitLIndex 2   = 1
findNextBitLIndex 4   = 2
findNextBitLIndex 8   = 3
findNextBitLIndex 16  = 4
findNextBitLIndex 32  = 5
findNextBitLIndex 64  = 6
findNextBitLIndex 128 = 7
findNextBitLIndex _   = 0

nextIndex :: Word8 -> Int
nextIndex x
    | x >= 7 = 0
    | otherwise = fromIntegral $ x + 1
