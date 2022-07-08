import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import System.Environment (getArgs)
import Control.Applicative (liftA2)
import Data.Either (fromRight)
import Control.Monad.ST
import Control.Monad
import Data.STRef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)
import Data.List (singleton)

type Graph = Map.Map Int (V.Vector Int)

readInput :: String -> IO Graph
readInput file = do
    let parse = fst . fromRight (-1, T.empty) . decimal
    edges <- map (liftA2 (,) head (singleton . (!! 1)) . map parse . T.words) . T.lines <$> TIO.readFile file
    return $ (Map.map V.fromList . Map.fromListWith (flip (++))) edges

reverseGraph :: Graph -> Graph
reverseGraph g = runST $ do
    out <- newSTRef (Map.empty :: Map.Map Int [Int])
    forM_ (Map.assocs g) $ \(k, vs) -> do
        V.forM_ vs $ \v -> do
            isMember <- Map.member v <$> readSTRef out
            unless isMember $ modifySTRef out (Map.insert v [])
            modifySTRef out (Map.adjust (++ [k]) v)
    Map.map V.fromList <$> readSTRef out

doWhile :: Monad m => m Bool -> m a -> m ()
doWhile cond act = do
    res <- cond
    when res (act >> doWhile cond act)

finishingTime :: Graph -> V.Vector Int
finishingTime g = runST $ do
    let magic = 900000 :: Int
    ft <- MV.replicate magic (-1 :: Int)
    visited <- MV.replicate magic False
    timer <- newSTRef (0 :: Int)
    stack <- newSTRef []

    let dfs = doWhile (not . null <$> readSTRef stack) $ do
        cur <- head <$> readSTRef stack
        modifySTRef stack tail
        if cur < 0 then do
            now <- readSTRef timer
            modifySTRef timer (1 +)
            MV.write ft now (-cur)
        else do
            seen <- MV.read visited cur
            unless seen $ do
                modifySTRef stack (-cur :)
                MV.write visited cur True
                when (Map.member cur g) $ do
                    let edges = V.reverse $ g Map.! cur
                    V.forM_ edges $ \edge -> do
                        edgeSeen <- MV.read visited edge
                        unless edgeSeen (modifySTRef stack (edge :))

    forM_ (reverse $ Map.keys g) $ \node -> do
        seen <- MV.read visited node
        unless seen $ do
            modifySTRef stack (node :)
            dfs

    endTime <- readSTRef timer
    let slice = MV.take endTime ft
    V.freeze slice

main :: IO ()
main = do
    input <- getArgs >>= readInput . head
    print $ Map.size input
    print $ (Map.size . reverseGraph) input
    print $ (V.length . finishingTime . reverseGraph) input
