import qualified Data.IntSet as IntSet
import Data.Either (fromRight)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe, isJust, fromJust)

import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Text as T

readInput :: String -> IO IntSet.IntSet
readInput file = IntSet.fromList
    . map (fst . fromRight (0, T.empty) . TR.signed TR.decimal)
    . T.lines <$> TIO.readFile file

twoSum :: IntSet.IntSet -> Int
twoSum set = (IntSet.size . foldl1 IntSet.union . map validSums . IntSet.toList) set
    where n = 10000
          lend = IntSet.findMin set
          rend = IntSet.findMax set

          validSums :: Int -> IntSet.IntSet
          validSums cur = let lb = fromMaybe lend (IntSet.lookupGE (-n - cur) set)
                              rb = fromMaybe (rend + 1) (IntSet.lookupLT (n - cur) set)
                              chunk = fst $ IntSet.split (rb + 1) (snd $ IntSet.split (lb - 1) set)
                              validSum it = if cur + it >= -n && cur + it <= n && cur /= it then IntSet.singleton (cur + it) else IntSet.empty
                          in (foldl IntSet.union IntSet.empty . map validSum . IntSet.toList) chunk

main :: IO ()
main = do
    input <- getArgs >>= readInput . head
    print (IntSet.size input)
    print (twoSum input)
    return ()
