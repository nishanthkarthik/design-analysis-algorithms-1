import System.Environment (getArgs)
import qualified Data.Vector.Unboxed as V
import Data.Either (fromRight)
import qualified Data.IntSet as IntSet
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Control.Applicative (liftA2)

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO

readInput :: FilePath -> IO (V.Vector Int)
readInput file = V.fromList . map (fst . fromRight (0, T.empty) . TR.signed TR.decimal) . T.lines <$> TIO.readFile file

data State = State {
    stream :: V.Vector Int,
    minHeap :: IntSet.IntSet,
    maxHeap :: IntSet.IntSet,
    runningSum :: Int } deriving (Eq, Show)

median :: IntSet.IntSet -> IntSet.IntSet -> Int
median l r
    | IntSet.null l && IntSet.null r = minBound
    | IntSet.null r = IntSet.findMax l
    | IntSet.null l = IntSet.findMin r
    | mod (IntSet.size l + IntSet.size r) 2 == 1 = if IntSet.size r > IntSet.size l then IntSet.findMin r else IntSet.findMax l
    | otherwise = IntSet.findMax l

medianStep :: State -> Int -> State
medianStep st i = runST $ do
    let e = stream st V.! i
    l <- newSTRef (maxHeap st)
    r <- newSTRef (minHeap st)

    pushLeft <- liftA2 (||) IntSet.null ((> e) . IntSet.findMin) <$> readSTRef r
    modifySTRef (if pushLeft then l else r) (IntSet.insert e)

    lsize <- IntSet.size <$> readSTRef l
    rsize <- IntSet.size <$> readSTRef r

    when (lsize > 1 && rsize < lsize - 1) $ do
        top <- IntSet.findMax <$> readSTRef l
        modifySTRef l (IntSet.delete top)
        modifySTRef r (IntSet.insert top)

    lsize' <- IntSet.size <$> readSTRef l
    rsize' <- IntSet.size <$> readSTRef r

    when (rsize' > 1 && lsize' < rsize' - 1) $ do
        top <- IntSet.findMin <$> readSTRef r
        modifySTRef r (IntSet.delete top)
        modifySTRef l (IntSet.insert top)

    l' <- readSTRef l
    r' <- readSTRef r

    return (st { maxHeap = l', minHeap = r', runningSum = mod (runningSum st + median l' r') 10000 })

main :: IO ()
main = do
    input <- getArgs >>= readInput . head
    print (V.length input)
    print $ runningSum (foldl medianStep (State input IntSet.empty IntSet.empty 0) [0..V.length input - 1])
