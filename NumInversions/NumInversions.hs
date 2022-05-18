import Data.Vector.Unboxed ((!), (//), Unbox, Vector, fromList, toList)
import qualified Data.Vector.Unboxed as V
import Test.HUnit
import Control.Monad.State

mergeSort :: (Ord a, Unbox a) => Vector a -> Vector a
mergeSort v
    | V.length v <= 1 = v
    | otherwise = fromList $ mergeLists (recurse l) (recurse r)
    where
        (l, r) = V.splitAt (V.length v `div` 2) v
        recurse = toList . mergeSort


mergeLists :: (Ord a) => [a] -> [a] -> [a]
mergeLists a [] = a
mergeLists [] b = b
mergeLists af@(a:as) bf@(b:bs)
    | a < b = a : mergeLists as bf
    | otherwise = b : mergeLists af bs


main :: IO Counts
main = runTestTT tests
    where
        fn :: [Int] -> [Int]
        fn = toList . mergeSort . fromList
        tests = test [  "reversed" ~: [1..10] ~=? fn [10,9..1],
                        "random" ~: [1..5] ~=? fn [4,1,5,2,3],
                        "empty" ~: [] ~=? fn [],
                        "singleton" ~: [1] ~=? fn [1],
                        "duplicates" ~: [1,2,3,3,4] ~=? fn [4, 1, 3, 2, 3],
                        "odd-even" ~: [1..7] ~=? fn [1,3,5,7,2,4,6] ]
