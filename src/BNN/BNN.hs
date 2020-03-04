----
---- BNN-Pipes, Copyright (C) 29/Sep/2018
---- Creator: rhvall
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

module BNN.BNN
-- (
--
-- )
where

import           Control.Monad                    (forever, void)
import           Data.Bool                        (bool)
import           Pipes
-- import qualified Pipes.Prelude                    as P
import           BNN.BNU
import           BNN.BNURandom
import           Control.Concurrent.STM
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Bits
import qualified Data.Vector                      as V
import qualified Pipes.Lift                       as PL
import           System.Random

-- type QueryOp = (QueryElem, BNUComp)
type UpdateOp = (QueryElem, Int)

data BNUElem = BNUElem {
    inU  :: BNUBType,
    outU :: BNUBType,
    bnuU :: BNUComp
} deriving (Eq, Show)

numberOfNeurons :: Int
numberOfNeurons = 3

numberOfIterations :: Int
numberOfIterations = 200

numberOfIterationsShow :: Int
numberOfIterationsShow = 1

queryElems :: [QueryElem]
-- queryElems = [QueryElem 1 10, QueryElem 2 20, QueryElem 3 30, QueryElem 4 40, QueryElem 5 50, QueryElem 6 60]
-- queryElems = [QueryElem 1 10, QueryElem 2 20, QueryElem 3 30, QueryElem 4 40, QueryElem 5 50]
-- XOR example
--queryElems = [QueryElem 0 1, QueryElem 1 0, QueryElem 2 0, QueryElem 3 1]
-- pseudo-sine example
queryElems = [QueryElem 45 85, QueryElem 46 90, QueryElem 47 123, QueryElem 48 76, QueryElem 49 95]
-- queryElems = [QueryElem 1 10, QueryElem 2 20, QueryElem 3 30, QueryElem 4 40]

produceData :: Int -> [QueryElem] -> Producer QueryElem IO ()
-- produceData = each [constructQE x | x <- [1,2,3,4]]
produceData quantity elems = each . concat $ replicate quantity elems
-- produceData = each [constructQE x | x <- [0 .. 254]]
--     where constructQE x = QueryElem x (x + 10)

-- produceData = each $ zipWith QueryElem input output
--     where input = [79,219,7,199,101,247,70,199]
--           output = [219,79,73,56,186,109,196,56]

produceDataLoop :: TVar Bool -> Producer QueryElem IO ()
produceDataLoop finishedTVar = do
    finished <- lift $ readTVarIO finishedTVar
    -- each $ until (const $ not finished) id lst
    each $ until (const finished) id lst
    -- each lst
    -- each $ until (const False) id lst
    where input = [79,219,7,199,101,247,70,199]
          output = [219,79,73,56,186,109,196,56]
          lst = zipWith QueryElem input output

queryBNUComp :: TVar BNUElem -> Pipe QueryElem QueryElem IO ()
queryBNUComp bnuElemT = forever $ do
    (QueryElem inputQ outputQ) <- await
    (BNUElem _ _ bnuUE) <- lift $ readTVarIO bnuElemT
    let bnuQ = bnuWT bnuUE
    let xorBNUQ = xorBNU bnuQ inputQ
    let outBNUQ = queryBNU bnuQ inputQ
    let bnuCU = BNUComp bnuQ (bnuUpdate bnuUE) xorBNUQ
    lift . atomically . writeTVar bnuElemT $ BNUElem inputQ outBNUQ bnuCU

    -- lift . putStrLn $ "QueryBNUComp"
    -- lift . putStrLn $ "Input:" ++ show inputQ
    -- lift . putStrLn $ "Expected:" ++ show outputQ
    -- lift . putStrLn $ "XOR:" ++ show xorBNUQ
    -- lift . putStrLn $ "Output:" ++ show outBNUQ
    -- lift . putStrLn $ "+++++++"

    yield $ QueryElem outBNUQ outputQ

calculateUpdate :: Int -> Pipe QueryElem UpdateOp IO ()
calculateUpdate numNeurons = flip evalStateT (1, Threshold) . forever $ do
    (QueryElem outputQ expected) <- lift await
    lastIndex <- get

    let toCount = updateTypeCounter numNeurons lastIndex
    let delta = xor outputQ expected

    -- lift . lift . putStrLn $ "LastIdx: " ++ show toCount
    -- lift . lift . putStrLn $ "Output: " ++ show outputQ
    -- lift . lift . putStrLn $ "Expected: " ++ show expected
    -- lift . lift . putStrLn $ "Delta: " ++ show delta

    lift . lift . putStrLn $ "*************************"
    lift . lift . putStrLn $ "O: " ++ show outputQ ++ " E: " ++ show expected ++ " D: " ++ show delta
    lift . lift . putStrLn $ "ToCount" ++ show toCount

    -- lift . lift . putStrLn $ "-----"

    bool (put lastIndex) (put toCount) (delta > 0)

    lift . yield $ (QueryElem expected delta, fst toCount)

updateBNUComp :: TVar BNUElem -> Pipe UpdateOp UpdateOp IO ()
updateBNUComp bnuCTVar = forever $ do
    -- upOp@(qElem, num) <- await
    -- case num of
    --     -- 0 -> yield upOp
    --     _ -> do
    (qElem, num) <- await
    bnuNC@(BNUElem inV outV bnuV) <- lift $ readTVarIO bnuCTVar
    let (bnuN, bnuNU) = updateBNU (bnuWT bnuV) (bnuUpdate bnuV) inV outV (outQ qElem)
    let bnuNC' = BNUComp bnuN bnuNU V.empty
    let nDelta = nextDelta bnuNC qElem
    lift . atomically . writeTVar bnuCTVar $ BNUElem 0 0 bnuNC'

    lift . putStrLn $ "ToUpdateBNUComp:"
    lift . putStrLn $ "Input:" ++ show inV
    lift . putStrLn $ "Output:" ++ show outV
    lift . putStrLn $ "Delta OutQ:" ++ show (outQ qElem)
    lift . putStrLn $ "Delta IntQ:" ++ show (inQ qElem)
    lift . putStrLn $ "BNUC:" ++ show bnuV
    lift . putStrLn $ "BNUU:" ++ show bnuNC'

    yield (nDelta, num - 1)
        -- _ -> do
        --     bnuC <- lift $ readTVarIO bnuCTVar
        --     let nDelta = nextDelta bnuC qElem
        --
        --     lift . putStrLn $ "ToDeltaUpdate:"
        --     -- lift . putStrLn $ "BNUC:" ++ show bnuC
        --     lift . putStrLn $ "NextD:" ++ show nDelta
        --     lift . putStrLn $ "-------"
        --
        --     yield (nDelta, num - 1)

-- showResults :: TVar Bool -> Consumer UpdateOp IO ()
-- showResults finished = forever $ do
-- showResults = forever $ do
showResults :: Proxy () UpdateOp () X IO ()
showResults = do
    (QueryElem _ delta, it) <- await
    lift . putStrLn $ "Delta: " ++ show delta
    lift . putStrLn $ "Iteration: " ++ show it
    -- case delta of
    --     0 -> do
    --         lift . putStrLn $ "Delta 0"
    --         lift . atomically $ writeTVar finished True
    --     _   -> return ()
    -- return ()
    -- lift $ putStrLn "Show Results"
    -- lift $ print qOpLst

pipedBNN :: Int -> IO (TVar BNUElem, TVar BNUElem, TVar BNUElem)
pipedBNN iterations = do
    -- let bnu0 = zeroBNUC
    -- bnuc0 <- newTVarIO $ BNUElem 0 0 bnu0
    -- bnuc1 <- newTVarIO $ BNUElem 0 0 bnu0
    -- bnuc2 <- newTVarIO $ BNUElem 0 0 bnu0

    let bnu0 = randomBNUC $ mkStdGen 4
    let bnu1 = randomBNUC $ mkStdGen 5
    let bnu2 = randomBNUC $ mkStdGen 6

    bnuc0 <- newTVarIO $ BNUElem 0 0 bnu0
    bnuc1 <- newTVarIO $ BNUElem 0 0 bnu1
    bnuc2 <- newTVarIO $ BNUElem 0 0 bnu2

    -- finishTVar <- newTVarIO False

    print "--------------------------------------"
    print "--------------------------------------"

    pipedTrain iterations bnuc0 bnuc1 bnuc2

    print "Learned Pattern"

    pipedQuery numberOfIterationsShow bnuc0 bnuc1 bnuc2

    print "Finished"

    return (bnuc0, bnuc1, bnuc2)

nextDelta :: BNUElem -> QueryElem -> QueryElem
nextDelta bElem qElem = QueryElem expected deltaOred
    where (QueryElem expected deltaU) = qElem
          (BNUElem _ outputQ bnuQ) = bElem
          deltaBack = deltaWeight (bnuXOR bnuQ) outputQ deltaU
          deltaOred = V.foldl (.|.) 0 deltaBack


pipedTrain :: Int -> TVar BNUElem -> TVar BNUElem -> TVar BNUElem -> IO ()
pipedTrain iterations bnuc0 bnuc1 bnuc2 = runEffect $
    produceData iterations queryElems >->
    --produceDataLoop finishTVar >->
    queryBNUComp bnuc0 >->
    queryBNUComp bnuc1 >->
    queryBNUComp bnuc2 >->
    calculateUpdate numberOfNeurons >->
    updateBNUComp bnuc2 >->
    updateBNUComp bnuc1 >->
    updateBNUComp bnuc0 >->
    showResults


pipedQuery :: Int -> TVar BNUElem -> TVar BNUElem -> TVar BNUElem -> IO ()
pipedQuery iterations bnuc0 bnuc1 bnuc2 = runEffect $
    produceData iterations queryElems >->
    --produceDataLoop finishTVar >->
    queryBNUComp bnuc0 >->
    queryBNUComp bnuc1 >->
    queryBNUComp bnuc2 >->
    calculateUpdate numberOfNeurons >->
    showResults -- finishTVar

----------------------------------------------------------------------
-------------- EXAMPLES ----------------------------------------------
----------------------------------------------------------------------

-- mainBNN :: IO ()
-- mainBNN = void $ runStateT code [1..]
-- --
-- -- layer an infinite list of uniques over the IO monad
-- --
--
-- code :: StateT [Integer] IO ()
-- code = do
--     x <- pop
--     io $ print x
--     y <- pop
--     io $ print y
--     return ()
--
-- --
-- -- pop the next unique off the stack
-- --
-- pop :: StateT [Integer] IO Integer
-- pop = do
--     (x:xs) <- get
--     put xs
--     return x
--
-- io :: IO a -> StateT [Integer] IO a
-- io = liftIO
--
--
-- exampleExp :: Monad m => Pipe Int Int (ExceptT String m) r
-- exampleExp = for cat $ \n ->
--     if n == 0
--     then lift $ throwE "Zero is forbidden"
--     else yield n
--
-- caught :: Pipe Int Int (ExceptT String IO) r
-- caught = exampleExp `PL.catchError` \str -> do
--     liftIO (putStrLn str)
--     caught
--
-- exampleState :: Monad m => Pipe Int Int (StateT Int m) r
-- exampleState = for cat $ \n -> do
--     val <- lift get
--     -- lift $ putStrLn (n + val)
--     lift . put $ val + n
--     yield val
