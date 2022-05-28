import System.Environment (getArgs)
import Control.Exception (assert)
import Control.Monad.ST (runST, ST)
import Control.Monad (when, forM_, when)
import Data.STRef

import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Data.List (sortBy)
import Data.Function (on)

readInput :: String -> IO (V.Vector Int)
readInput file = V.fromList . map read . filter (not . null) . lines <$> readFile file

type FindPivot s a = V.Vector a -> Int -> Int -> Int

pivotLeft :: (Ord a) => FindPivot s a
pivotLeft _ l _ = l

pivotRight :: (Ord a) => FindPivot s a
pivotRight _ _ r = r

pivotMedian :: (Ord a, V.Unbox a) => FindPivot s a
pivotMedian v l r = (fst . head . tail) $ sortBy (compare `on` snd) (zip arr (map (v V.!) arr))
    where arr = [l, r, (l + r) `div` 2]

partition :: (Ord a, V.Unbox a) => FindPivot s a -> MV.MVector s a -> Int -> Int -> ST s Int
partition p v l r = do
    pivotInput_ <- V.freeze v
    let pivot = p pivotInput_ l r
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

quickSort_ :: (Ord a, V.Unbox a) => FindPivot s a -> MV.MVector s a -> Int -> Int -> ST s Comparisons
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

quickSort :: (Ord a, V.Unbox a) => FindPivot s a -> V.Vector a -> (V.Vector a, Comparisons)
quickSort p v = runST $ do
    work_ <- V.thaw v
    comps <- quickSort_ p work_ 0 (V.length v - 1)
    sorted <- V.freeze work_
    return (sorted, comps)

main :: IO ()
main = do
    argFile <- (\x -> if null x then "QuickSort/input.txt" else head x) <$> getArgs
    input <- readInput argFile
    putStrLn ((("PivotLeft " ++) . show . snd . quickSort pivotLeft) input)
    putStrLn ((("PivotRight " ++) . show . snd . quickSort pivotRight) input)
    putStrLn ((("PivotMedian " ++) . show . snd . quickSort pivotMedian) input)
    return ()
