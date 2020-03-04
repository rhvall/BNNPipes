md = mnistData
seed <- randomIO :: IO Int
pop = defaultParamsMNIST seed
(pop', sol) <- mnistLoop pop iterationsLoop
let score = gScorer mnistFit sol
-- score
-- pop'
-- sol
sample = snd $ md !! 1
preRes = pushThrough (mkPhenotype sol) sample
res = map (\x -> bool x 0 $ x < 0.1 ) preRes
res


-- dotPath = "./dotSolMNIST.dot"

-- writeGenomeDot dotPath sol