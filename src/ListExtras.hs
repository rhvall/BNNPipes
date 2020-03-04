----
---- CNN-PhD version 0.1, Copyright (C) 24/Jul/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Helper file to add random creation of CAN types
module ListExtras
-- (
-- )
where

import           Data.Bool (bool)

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

shiftLeft :: [a] -> [a]
shiftLeft []  = []
shiftLeft [x] = [x]
shiftLeft x   = tail x ++ [head x]

shiftRight :: [a] -> [a]
shiftRight []  = []
shiftRight [x] = [x]
shiftRight x   = last x : init x

applyNTimes :: Int -> (b -> b) -> b -> b
applyNTimes = (foldr (.) id .) . replicate

binaryList :: Integral a => [a] -> [Bool]
binaryList = map (\x -> bool False True (0 /= x))

toBoth :: (a -> b) -> (a, a) -> (b, b)
toBoth f (x, y) = (f x, f y)

num2Bin' :: Int -> Int -> [Bool]
num2Bin' size 0 = replicate size False
num2Bin' size num = map (\x -> bool False True (check x == 0)) $ lst size
    where lst x = take x $ iterate (* 2) 1
          check x = mod (div (num - x) x) 2

n2b :: Int -> [Bool]
n2b = num2Bin' 8

bin2Num :: [Bool] -> Int
bin2Num [] = 0
bin2Num lst
    | last lst = foldl (\z (x,y) -> z - condInv x y ) (-1) lstI
    | otherwise = foldl (\z (x,y) -> z + cond x y ) 0 lstI
    where lstI = zip [0 .. ] lst
          cond x = bool 0 (2 ^ x)
          condInv x = bool (2 ^ x) 0

-- variant of map that passes each element's index as a second argument to f
indexedMap :: (a -> Int -> b) -> [a] -> [b]
indexedMap f l = zipWith f l [0..]

replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] _ _ = []
replaceElem x 0 y = y : tail x
replaceElem x n val
    | n >= length x = x
    | otherwise = xs ++ val : ys
        where (xs, _:ys) = splitAt n x

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
    where (as,bs) = splitAt n xs
