import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

import Control.Applicative (liftA2)
import System.Environment (getArgs)
import Data.Either (fromRight)
import Control.Monad
import Control.Monad.ST
import Data.STRef

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

type Graph = IntMap.IntMap (IntMap.IntMap Int)

readInput :: String -> IO Graph
readInput file = do
    let extractRight = fst . fromRight (0, T.empty) . TR.decimal
        parseEdge = liftA2 (,) head (!! 1) . map extractRight . T.split (== ',')
        key = extractRight . head
        val = IntMap.fromList . map parseEdge . tail
    IntMap.fromList
        . map (liftA2 (,) key val . T.words)
        . T.lines <$> TIO.readFile file


doWhile :: Monad m => m Bool -> m () -> m ()
doWhile cond act = cond >>= (\res -> when res (act >> doWhile cond act))


shortestPath :: Graph -> Int -> V.Vector Int
shortestPath graph start = runST $ do
    let maxSize = 1 + fst (IntMap.findMax graph)
    visited <- MV.replicate maxSize False
    dist <- MV.replicate maxSize (maxBound :: Int)

    -- distance -> node
    pqueue <- newSTRef Set.empty

    MV.write dist start 0
    modifySTRef pqueue (Set.insert (0, start))

    doWhile (not . Set.null <$> readSTRef pqueue) $ do
        minItem@(minDist, minNode) <- Set.findMin <$> readSTRef pqueue
        modifySTRef pqueue (Set.delete minItem)
        seen <- MV.read visited minNode

        unless seen $ do
            let edges = graph IntMap.! minNode
            forM_ (IntMap.assocs edges) $ \(edge, edgeLen) -> do
                nodeDist <- MV.read dist minNode
                edgeDist <- MV.read dist edge
                let newDist = min edgeDist (nodeDist + edgeLen)
                MV.write dist edge newDist
                modifySTRef pqueue (Set.insert (newDist, edge))
            MV.write visited minNode True

    V.freeze dist

printSolutions :: [Int] -> V.Vector Int -> IO ()
printSolutions indices v = forM_ indices $ \i -> when (i < V.length v) (print (i, v V.! i))

main :: IO ()
main = do
    input <- getArgs >>= readInput . head
    let dists = shortestPath input 1
    print $ IntMap.size input
    printSolutions [7, 37, 59, 82, 99, 115, 133, 165, 188, 197] dists
