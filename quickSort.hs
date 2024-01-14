import Control.Monad.ST
import Control.Monad
import Data.STRef

main :: IO ()
main = do
    let xs = reverse [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    print $ runST $ do
        xsRef <- newSTRef xs
        quickSort xsRef
        read' xsRef

-- para desempenho usar MTArray
swap :: [a] -> Int -> Int -> [a]
swap xs i' j' = [ if i == i' then xs !! j' else
                  if i == j' then xs !! i' else
                   x | (i, x) <- zip [0..] xs]


(.=) = writeSTRef
(.=!) = modifySTRef
read' = readSTRef 

quickSort :: (Ord a) => STRef s [a] -> ST s ()
quickSort xsRef = do
    len <- length <$> read' xsRef
    auxSort 0 (len - 1)
    where
    auxSort lo hi = when (lo < hi) $ do
            mid <- partition xsRef lo hi
            auxSort lo (mid - 1)
            auxSort (mid+1) hi

partition :: Ord a => STRef s [a] -> Int -> Int -> ST s Int
partition xs lo hi = do
    pivot <- (!! hi) <$> read' xs
    i <- newSTRef lo

    forM_ [lo..hi] $ \j -> do
        xj <- (!! j) <$> read' xs
        when (xj <= pivot) $ do
            i'  <- read' i
            xs' <- read' xs
            xs .= swap xs' j i'
            i .=! succ

    i .=! pred
    read' i

