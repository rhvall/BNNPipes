----
---- BNN-Pipes, Copyright (C) 27/Feb/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE BangPatterns    #-}

{-# LANGUAGE RecordWildCards #-}

module Evol.EvoCom
-- (
--
-- )
where

import           BNN.BNN16
import           BNN.BNP16
import           BNN.BNU16
import           BNN.BNU16Random
import           BNN.BNUEvo                  (mateBNN16, mutateBNP16Size)
import           BNN.VectorUtilities         (sortUsingVec)
import           Control.Monad.Loops
import           Control.Monad.Random.Strict
import           Data.Bits
import           Data.Bool                   (bool)
import qualified Data.Vector                 as V
import           Evol.EvoBNN

data ComQuery = ComQuery {
    cqInput  :: {-# UNPACK #-} !BNNInput,
    cqPass   :: {-# UNPACK #-} !BNNInput,
    cqOutput :: {-# UNPACK #-} !BNPOutput
} deriving (Show)

data LoopDta = LoopDta {
    ldIte  :: {-# UNPACK #-} !Int,
    ldInfo :: {-# UNPACK #-} !PopInfo,
    ldSco  :: {-# UNPACK #-} !Scores,
    ldAl   :: {-# UNPACK #-} !BNP16,
    ldBo   :: {-# UNPACK #-} !BNP16
} deriving (Show)

defaultPopCom :: PopInfo
defaultPopCom = PopInfo 11 21 128 128 10 12 5000
-- 11       21         16       128      10       12        50
-- sizeBNU, sizeLayer, sizeNet, sizePop, elitism, mutation, maxGens

secComAB :: PopInfo -> IO (BNP16, BNP16)
secComAB popInfo = do
    let popCreator = randomBNP16 (sizeBNU popInfo) (sizeLayer popInfo) (sizeNet popInfo) (sizePop popInfo)
    print "Creating Alice"
    al <- evalRandIO popCreator
    print "Creating Bob"
    bo <- evalRandIO popCreator
    return (al, bo)

queryPops :: RandomGen g => Int -> BNP16 -> BNP16 -> Rand g ComQuery
queryPops inSize al bo = do
    !input <- V.fromList . take inSize <$> getRandomRs (0, 4096)
    !pass <- V.fromList . take inSize <$> getRandomRs (0, 4096)
    let packed = input V.++ pass
    let alOut = queryBNP16 packed al
    let packed2 = V.map (V.++ pass) alOut
    let !boOut = V.zipWith queryBNN16 packed2 al
    return $ ComQuery input pass boOut

distComQ :: ComQuery -> Scores
distComQ (ComQuery ci _ co) = V.map (V.sum . V.zipWith xor ci) co

evolveCom :: RandomGen g => PopInfo -> Scores -> BNP16 -> Rand g BNP16
evolveCom popInfo score pop = do -- ((info, final), g3)
    let sorted = sortUsingVec pop score
        popSize = sizePop popInfo
    !children <- eliteChildren sorted (elitism popInfo)
    !mutated <- mutateBNP16Size sorted (mutation popInfo)
    let partial = V.take popSize $ V.singleton (V.head sorted) V.++ children V.++ mutated
        partialLength = V.length partial
    !appendNew <- randomBNP16 (sizeBNU popInfo) (sizeLayer popInfo) (sizeNet popInfo) (popSize - partialLength)
    let !final = bool partial (partial V.++ appendNew) (partialLength < popSize)
    return final

loopAction :: RandomGen g => LoopDta -> Rand g (Bool, LoopDta)
loopAction ld@LoopDta{ .. }
    | ldIte <= 0 = return (True, ld)
    | checkScores ldSco = return (True, ld)
    | otherwise = do
        !evoAl <- evolveCom ldInfo ldSco ldAl
        !evoBo <- evolveCom ldInfo ldSco ldBo
        !sco' <- distComQ <$> queryPops (sizeBNU ldInfo) evoAl evoBo
        return (False, LoopDta (ldIte - 1) ldInfo sco' evoAl evoBo)

checkScores :: Scores -> Bool
checkScores = not . V.null . V.filter (== 0)

scoreMean :: Scores -> String
scoreMean x = show $ fromIntegral (V.sum x) / fromIntegral (V.length x)

mainCom :: PopInfo -> IO (BNP16, BNP16)
mainCom popInfo = do
    (al, bo) <- secComAB popInfo
    cq <- evalRandIO $ queryPops (sizeBNU popInfo) al bo
    let loopDta = LoopDta (maxGens popInfo) popInfo (distComQ cq) al bo
    let prLD x = print $ "Gen: " ++ show (ldIte x) ++ ", Score: " ++ show (scoreMean $ ldSco x)
    (_, ld) <- iterateUntilM fst (\(_, x) -> prLD x >> evalRandIO (loopAction x)) (False, loopDta)
    return (ldAl ld, ldBo ld)
