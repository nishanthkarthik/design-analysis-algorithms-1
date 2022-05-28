import System.Environment (getArgs)
import Control.Exception (assert)
import Control.Monad.ST (runST, ST)
import Control.Monad (when, forM_, when)
import Data.STRef

import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Data.List (sortOn)

readInput :: String -> IO (V.Vector Int)
readInput file = V.fromList . map read . filter (not . null) . lines <$> readFile file

type FindPivot s a = V.Vector a -> Int -> Int -> Int

data Pivot = PivotLeft | PivotRight | PivotMedian deriving (Show, Eq)

partition :: (Ord a, V.Unbox a) => Pivot -> MV.MVector s a -> Int -> Int -> ST s Int
partition p v l r = do
    pivot <- case p of
            PivotLeft -> return l
            PivotRight -> return r
            PivotMedian -> do
                let indices = [l, r, (l + r) `div` 2]
                values <- mapM (MV.read v) indices
                return $ (fst . (!! 1) . sortOn snd) (zip indices values)
    when (pivot /= l) $ MV.swap v pivot l
    feed <- newSTRef (l + 1)
    forM_ [l + 1 .. r] $ \i -> do
        lcomp <- MV.read v i
        rcomp <- MV.read v l
        when (lcomp < rcomp) $ do
            feed_ <- readSTRef feed
            MV.swap v feed_ i
            writeSTRef feed (feed_ + 1)
    feed_ <- readSTRef feed
    MV.swap v l (feed_ - 1)
    return feed_

type Comparisons = Int

quickSort_ :: (Ord a, V.Unbox a) => Pivot -> MV.MVector s a -> Int -> Int -> ST s Comparisons
quickSort_ p v l r = do
    let window = r - l + 1
        numCompares = max 0 (window - 1)
    if window <= 1 then
        return numCompares
    else do
        feed <- partition p v l r
        leftCompares <- quickSort_ p v l (feed - 2)
        rightCompares <- quickSort_ p v feed r
        return (numCompares + leftCompares + rightCompares)

quickSort :: (Ord a, V.Unbox a) => Pivot -> V.Vector a -> (V.Vector a, Comparisons)
quickSort p v = runST $ do
    work_ <- V.thaw v
    comps <- quickSort_ p work_ 0 (V.length v - 1)
    sorted <- V.freeze work_
    return (sorted, comps)

main :: IO ()
main = do
    argFile <- (\x -> if null x then "QuickSort/input.txt" else head x) <$> getArgs
    input <- readInput argFile
    putStrLn ((("PivotLeft " ++) . show . snd . quickSort PivotLeft) input)
    putStrLn ((("PivotRight " ++) . show . snd . quickSort PivotRight) input)
    putStrLn ((("PivotMedian " ++) . show . snd . quickSort PivotMedian) input)
    return ()
