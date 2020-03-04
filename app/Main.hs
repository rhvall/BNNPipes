----
---- BNN-Pipes, Copyright (C) 20/Sep/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

module Main where

import           Data.Time   (diffUTCTime, getCurrentTime)
import           Evol.EvoCom

main :: IO ()
main = do
    startTime <- getCurrentTime
    putStrLn $ "Started at: " ++ show startTime
    (_, m) <- mainCom defaultPopCom
    print "Finished experiment"
    endTime <- getCurrentTime
    let diff = diffUTCTime endTime startTime
    putStrLn $ "Ended at: " ++ show endTime
    putStrLn $ "Difference: " ++ show diff
    print m
