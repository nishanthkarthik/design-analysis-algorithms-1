import System.Environment (getArgs)

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import System.Random.Stateful
import System.Random
import Control.Monad

type Group = IntSet.IntSet
type AdjList = Map.Map Group (Set.Set Group)

contractGraph :: StatefulGen g m => AdjList -> g -> m AdjList
contractGraph g gen = do
    rand1 <- uniformRM (0, Map.size g - 1) gen
    let (selK, selVs) = Map.elemAt rand1 g
    rand2 <- uniformRM (0, Set.size selVs) gen
    let selV = Set.elemAt 0 selVs
        mergedK = IntSet.union selK selV
        mergedV = Set.union selVs (g Map.! selV) Set.\\ Set.fromList [selK, selV]
        updatedG = foldr Map.delete (Map.insert mergedK mergedV g) [selV, selK]
        updateRefs key graph = foldl (\g_ it -> if Set.member key (g_ Map.! it) then Map.insert it (Set.delete key (Set.insert mergedK (g_ Map.! it))) g_ else g_) graph mergedV
    return $ foldr updateRefs updatedG [selK, selV]

contractAll' :: StatefulGen g m => AdjList -> g -> m AdjList
contractAll' graph gen = do
    if Map.size graph <= 2
       then return graph
       else do
           g' <- contractGraph graph gen
           contractAll' g' gen

contractAll :: RandomGen g => (AdjList, g) -> (AdjList, g)
contractAll (graph, gen) = runStateGen gen (contractAll' graph)

numCuts :: AdjList -> AdjList -> Int
numCuts ref g = sum [IntSet.size $ IntSet.intersection (IntSet.unions (ref Map.! IntSet.singleton a)) r | a <- IntSet.toList l]
    where (l, r) = (head (Map.keys g), Map.keys g !! 1)

minCut :: AdjList -> Int -> Int
minCut g reps = runStateGen_ (mkStdGen 1) $ \gen -> do
    minimum . map (numCuts g) <$> replicateM reps (contractAll' g gen)

readInput :: String -> IO AdjList
readInput file = do
    let toNode (x:xs) = (IntSet.singleton x, (Set.fromList . map IntSet.singleton) xs)
    Map.fromList . map (toNode . map read . words) . lines <$> readFile file

main = do
    argFile <- head <$> getArgs
    iters <- read . (!! 1) <$> getArgs
    input <- readInput argFile
    print (minCut input iters)
