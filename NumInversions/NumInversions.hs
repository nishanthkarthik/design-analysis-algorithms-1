import Test.HUnit
import Control.Monad.State
import Control.Applicative (liftA2)

data MergeResult a = MergeResult { inversions :: Integer, result :: [a] } deriving (Show)

mergeSort :: (Ord a) => [a] -> MergeResult a
mergeSort v
    | length v <= 1 = MergeResult 0 v
    | otherwise = MergeResult (inversions lmerged + inversions rmerged + splitinv) arr
    where (l, r) = splitAt (length v `div` 2) v
          lmerged = mergeSort l
          rmerged = mergeSort r
          (splitinv, arr) = mergeListsInv ((fromIntegral . length . result) lmerged, result lmerged) (result rmerged)

mergeListsInv :: (Ord a) => (Integer, [a]) -> [a] -> (Integer, [a])
mergeListsInv (_, a) [] = (0, a)
mergeListsInv (_, []) b = (0, b)
mergeListsInv (na, af@(a:as)) bf@(b:bs)
    | a < b = (invl, a : arl)
    | otherwise = (na + invr, b : arr)
    where (invl, arl) = mergeListsInv (na - 1, as) bf
          (invr, arr) = mergeListsInv (na, af) bs

data Input = Input { reference :: Integer, input :: [Int] } deriving (Show)

readInput :: String -> IO [Input]
readInput file = map (toInput . map read . words) . filter (not . null) . lines <$> readFile file
    where toInput = liftA2 Input (fromIntegral . head) tail

testSorted = map (TestCase . assertBool "Array is sorted" . isSorted . sortedFn . input)
    where sortedFn = result . mergeSort
          isSorted = and . liftA2 (zipWith (<=)) id tail

testInverse = map (("Inversions" ~:) . liftA2 (~=?) (fromIntegral . reference) (inversions . mergeSort . input))

main :: IO Counts
main = do
    let allTests = liftA2 (++) testSorted testInverse
    readInput "NumInversions/tests.txt" >>= runTestTT . TestList . allTests
    readInput "NumInversions/IntegerArraySpaced.txt" >>= runTestTT . TestList . allTests
