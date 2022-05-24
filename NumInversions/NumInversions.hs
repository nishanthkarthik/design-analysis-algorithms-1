import Test.HUnit
import Control.Monad.State
import Control.Applicative (liftA2)

mergeSort :: (Ord a) => [a] -> (Integer, [a])
mergeSort v
    | length v <= 1 = (0, v)
    | otherwise = (linv + rinv + splitinv, arr)
    where (l, r) = splitAt (length v `div` 2) v
          (linv, larr) = mergeSort l
          (rinv, rarr) = mergeSort r
          (splitinv, arr) = mergeListsInv (fromIntegral $ length larr, larr) rarr

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
    where sortedFn = snd . mergeSort
          isSorted xs = and $ zipWith (<=) xs (tail xs)

testInverse = map (\(inv, arr) -> "Inversions" ~: fromIntegral inv ~=? inverseFn arr)
    where inverseFn = fst . mergeSort


main :: IO Counts
main = do
    let allTests = liftA2 (++) testSorted testInverse
    readInput "NumInversions/tests.txt" >>= runTestTT . TestList . allTests
    readInput "NumInversions/IntegerArraySpaced.txt" >>= runTestTT . TestList . allTests
