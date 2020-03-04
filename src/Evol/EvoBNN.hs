----
---- BNN-Pipes, Copyright (C) 23/Dec/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- Moduke that evolves BNN to find the correct values until a given number
-- of generations have passed or the exact match between the output of a
-- network and the target function has been reached. It tries to replace
-- backpropagation for BNNs using evolutionary and genetic principles.

{-# LANGUAGE CPP #-}

module Evol.EvoBNN
-- (
--
-- )
where

import           BNN.BNN16                   (BNN16, BNNOutput, Query16 (..),
                                              distanceBNN16)
import           BNN.BNP16                   (BNP16, BNPQuery, BNPQueryVec)
import           BNN.BNU16                   (BNU16Type)
import           BNN.BNU16Random             (randomBNP16)
import           BNN.BNUEvo                  (mateBNN16, mutateBNP16Size)
import           BNN.VectorUtilities         (foldGeneric, pairElements,
                                              sortUsingVec)
import qualified Control.Monad.Loops         as ML
import           Control.Monad.Random.Strict (Rand, RandomGen, evalRand,
                                              evalRandIO, getRandomR,
                                              getRandomRs, mkStdGen, split)
import           Data.Bool                   (bool)
import qualified Data.Vector                 as V
import           Debug.Trace                 (trace)
import           Evol.SelectBNP              (createPopByMut)


import           BNN.BNN16                   (BNNInput, QueryVec (..))
import           BNN.BNP16                   (queryBNP16)
import           BNN.VectorUtilities         (splitChunks)

-- General configuration for the evolutionary algorithm, each item has an
-- effect on size, iterations or best match selection
data PopInfo = PopInfo {
    sizeBNU   :: {-# UNPACK #-} !Int, -- The number of elements inside a BNU
    sizeLayer :: {-# UNPACK #-} !Int, -- The number of layers a network will have to create a BNNLayer
    sizeNet   :: {-# UNPACK #-} !Int, -- Size of the network that creates a BNN
    sizePop   :: {-# UNPACK #-} !Int, -- The size of the population, composed of BNNs
    elitism   :: {-# UNPACK #-} !Int, -- Select a vector of best elements to be parents of the next generation
    mutation  :: {-# UNPACK #-} !Int, -- How many BNNs will be selected to mutate
    maxGens   :: {-# UNPACK #-} !Int  -- Maximum number of generations the algorithm should run
} deriving (Show)

-- Synonymn to a tuple that has a Population info and a BNP
type Population = (PopInfo, BNP16)

-- Synonymn to a fitness function that takes BNPQueryVec, BNN16 and returns
-- an integer, it is used to simplify function signatures only
type FitnessFunction = (BNPQueryVec -> BNN16 -> Int)

-- Synonymn to identify a vector of BNU16Types
type Scores = V.Vector BNU16Type

-- Constant value to a generic population info
defaultPop :: PopInfo
defaultPop = PopInfo 3 2 3 32 8 26 1024
-- 3        2           3       32          8       26      1024
-- sizeBNU, sizeLayer, sizeNet, sizePop, elitism, mutation, maxGens

-- -- This is the desired input / output pair, given that a set
-- -- of input elements correspond to a matched output.
-- target :: BNPQueryVec
-- target = V.fromList [Query16 0 1, Query16 1 0, Query16 2 0, Query16 3 1]

-- Given a target and a network, calculate how much it assimilates given
-- the distance between the network response and the desired output
fitnessNet :: FitnessFunction
fitnessNet queries bnn = V.foldl' (\x y -> x + fromIntegral (V.sum y)) 0 bnuOut
    where bnuOut = V.map (`distanceBNN16` bnn) queries

--------------------------------------------------------------------------------
------------------------------- Selection --------------------------------------
--------------------------------------------------------------------------------

-- -- From a population and a target of input/output values, ask randomly
-- -- which are the best BNN that has either a minimum/maximum fitness score
-- batchedBestSelection :: RandomGen g => g -> BNP16 -> Int -> BNPQuery -> (BNN16, g)
-- batchedBestSelection seed bnp16 batch expected = (batched V.! selectedIdx, snd $ next seed)
--     where batched = selectBatch seed bnp16 batch
--           -- selecte(dIdx = V.maxIndex $ V.map (fitness expected) batched
--           selectedIdx = V.minIndex $ V.map (fitnessNet expected) batched
--
-- -- Helper function to select randomly from a population a batch size given
-- -- by the population variable "bestBatch"
-- selectBatch :: RandomGen g => g -> BNP16 -> Int -> BNP16
-- selectBatch seed bnp batch = V.map (\x -> bnp V.! x) idxes
--     where idxes = V.fromList . take batch $ randomRs (0, V.length bnp) seed
--
-- -- Using population configuration "bestBatch", it will select the best candidates
-- -- in a pool to mate them and return the best two suited for a fitness function,
-- -- along with the updated seed.
-- selectParents :: RandomGen g => g -> BNP16 -> Int -> BNPQuery -> (BNN16, BNN16, g)
-- selectParents seed bnp16 batch expected = (parentX, parentY, g')
--     where (parentX, g) = batchedBestSelection seed bnp16 batch expected
--           (parentY, g') = batchedBestSelection g bnp16 batch expected


-- Given a full population, will take a subset with "size" length, which will
-- form a vector of elite BNN16, then create random pairs that would form the
-- basis of "parents" to create "children" networks and return that along with
-- the changes to the random generator
eliteChildren :: RandomGen g => BNP16 -> Int -> Rand g BNP16
eliteChildren bnp size = do
    let eliteVec = V.take size bnp
    eliteParentsIdx <- V.fromList . take size <$> getRandomRs (0, size - 1)
    let paired = pairElements eliteVec eliteParentsIdx
    return $ V.map (uncurry mateBNN16) paired

--------------------------------------------------------------------------------
------------------------------- Evolution --------------------------------------
--------------------------------------------------------------------------------

-- Main evolutionary function that takes a "population" and a "expected" vector
-- of values that a BNP16 should fulfill. The general process goes as follows:
-- 1.- Query every BNN16 element given a fitness function
-- 2.- Sort the BNP16 given the scores obtained
-- 3.- Mutate BNN16 according to the configuration
-- 4.- Mate elite BNN16 according to the configuration
-- 5.- Construct the population by the following structure: the first element
-- is the highest BNN16, then children of the elite, then mutations and lastly
-- (if space permits), new random values added to the last section of the vector
evolve :: RandomGen g => FitnessFunction -> BNPQueryVec -> Population -> Rand g Population
evolve fitness expected pop@(info, bnp) = do -- ((info, final), g3)
    let bnpSrcs = V.map (fitness expected) bnp
        sorted = sortUsingVec bnp bnpSrcs
        popSize = sizePop info
    children <- eliteChildren sorted (elitism info)
    mutated <- mutateBNP16Size sorted (mutation info)
    let partial = V.take popSize $ V.singleton (V.head sorted) V.++ children V.++ mutated
        partialLength = V.length partial
    appendNew <- randomBNP16 (sizeBNU info) (sizeLayer info) (sizeNet info) (popSize - partialLength)
    let final = bool partial (partial V.++ appendNew) (partialLength < popSize)
    return (info, final)

-- Main evolutionary function that takes a "population" and a "expected" vector
-- of values that a BNP16 should fulfill. The general process goes as follows:
-- 1.- Query every BNN16 element given a fitness function
-- 2.- Sort the BNP16 given the scores obtained
-- 3.- Mutate the "best" scored BNN16 (first element) an create a whole
-- population given the "sizePop" configuration
evolveMut :: RandomGen g => FitnessFunction -> BNPQueryVec -> Population -> Rand g Population
evolveMut fitness expected pop@(info, bnp) = do
    let bnpScores = V.map (fitness expected) bnp
    let sorted = sortUsingVec bnp bnpScores
    pop' <- createPopByMut (V.head sorted) (sizePop info)
    return (info, pop')

runEvolutionInfo :: RandomGen g => FitnessFunction -> BNPQueryVec -> PopInfo -> Rand g Population
runEvolutionInfo fitness expected popInfo = bnp >>= \x -> runEvolution fitness expected (popInfo, x)
    where bnp = randomBNP16 (sizeBNU popInfo) (sizeLayer popInfo) (sizeNet popInfo) (sizePop popInfo)

-- Main function to run the evolutionary algorithm, it takes a fittness "function",
-- "expected" values and configuration information to create a population that
-- tries to approach the network structure given the details provided.
runEvolution :: RandomGen g => FitnessFunction -> BNPQueryVec -> Population -> Rand g Population
runEvolution fitness expected (popInfo, bnp) = do
    let conditional (x, y) = bool False True (fitness expected (V.head $ snd x) == 0 || y >= maxGens popInfo)
#ifdef DEBUG
    (pop', gens) <- ML.iterateUntilM conditional (iterationDebug fitness expected) ((popInfo, bnp), 0)
#else
    -- let iteration (pop, gen) = evolveMut fitness expected pop >>= \x -> return (x, gen + 1)
    let iteration (pop, gen) = evolve fitness expected pop >>= \x -> return (x, gen + 1)
    (pop', _) <- ML.iterateUntilM conditional iteration ((popInfo, bnp), 0)
#endif
    return pop'

-- Helper function to stdout information about the generation number, Population
-- fitness and Top 5 scores. If the flag "DEBUG" is disabled, it will not show
-- these lines.
iterationDebug :: RandomGen g => FitnessFunction -> BNPQueryVec -> (Population, Int) -> Rand g (Population, Int)
iterationDebug fitness expected (pop, generation) = do
    let scores = V.map (fitness expected) $ snd pop
        sortedScore = sortUsingVec scores scores
        popFitness = V.sum scores
        traceStr = "Gen:" ++ show generation ++  ", Population Fitness: " ++ show popFitness
                   ++ "\nTop 5 Scores: " ++ show (V.take 5 sortedScore)
                   -- ++ "\nTop 5 Pop: " ++ show (V.take 5 $ snd pop)
        traceDbg = trace traceStr pop
    pop' <- evolveMut fitness expected traceDbg
    -- pop' <- evolve fitness expected traceDbg
    return (pop', generation + 1)


--------------------------------------------------------------------------------
----------------------------------- TEST ---------------------------------------
--------------------------------------------------------------------------------

mnistToQVec :: V.Vector (Int, V.Vector Int) -> V.Vector QueryVec
mnistToQVec = V.map mapper
    where toBNUT x = V.map fromIntegral x :: BNNInput
          toBNUTV x = V.singleton $ fromIntegral x
          mapper (x, y) = QueryVec (toBNUT y) (toBNUTV x)
