import Data.Vector.Unboxed ((!), (//), Unbox, Vector, fromList, toList)
import qualified Data.Vector.Unboxed as V
import Test.HUnit
import Control.Monad.State
import Control.Applicative (liftA2)

mergeSort :: (Ord a, Unbox a) => Vector a -> (Integer, Vector a)
mergeSort v
    | V.length v <= 1 = (0, v)
    | otherwise = (linv + rinv + splitinv, fromList arr)
    where (l, r) = V.splitAt (V.length v `div` 2) v
          (linv, larr) = mergeSort l
          (rinv, rarr) = mergeSort r
          (splitinv, arr) = mergeListsInv (fromIntegral $ V.length larr, toList larr) (toList rarr)

mergeListsInv :: (Ord a) => (Integer, [a]) -> [a] -> (Integer, [a])
mergeListsInv (_, a) [] = (0, a)
mergeListsInv (_, []) b = (0, b)
mergeListsInv (na, af@(a:as)) bf@(b:bs)
    | a < b = (invl, a : arl)
    | otherwise = (na + invr, b : arr)
    where (invl, arl) = mergeListsInv (na - 1, as) bf
          (invr, arr) = mergeListsInv (na, af) bs

readInput :: String -> IO [(Integer, [Int])]
readInput file = map (headTail . map read . words) . filter (not . null) . lines <$> readFile file
    where headTail (x:xs) = (fromIntegral x, xs)

testSorted = map (TestCase . assertBool "Array is sorted" . isSorted . sortedFn . snd)
    where sortedFn = toList . snd . mergeSort . fromList
          isSorted xs = and $ zipWith (<=) xs (tail xs)

testInverse = map (\(inv, arr) -> "Inversions" ~: fromIntegral inv ~=? inverseFn arr)
    where inverseFn = fst . mergeSort . fromList


main :: IO Counts
main = do
    let allTests = liftA2 (++) testSorted testInverse
    readInput "NumInversions/tests.txt" >>= runTestTT . TestList . allTests
    readInput "NumInversions/IntegerArraySpaced.txt" >>= runTestTT . TestList . allTests
