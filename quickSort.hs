import Control.Monad.ST
import Control.Monad
import Data.STRef

import Imp

main :: IO ()
main = do
    let xs = reverse [10, 2, 3, 5, 4, 7, 9, 8, 1, 0]
    print $ run $ do
        xsRef <- newSTRef xs
        quickSort xsRef
        yield xsRef

-- para desempenho usar MTArray
swap :: [a] -> Int -> Int -> [a]
swap xs i' j' = [ if i == i' then xs !! j' else
                  if i == j' then xs !! i' else
                   x | (i, x) <- zip [0..] xs]


quickSort :: (Ord a) => STRef s [a] -> ST s ()
quickSort xsRef = do
    len <- length <$> yield xsRef
    auxSort 0 (len - 1)
    where
    auxSort lo hi =
        when (lo < hi) $ do
            mid <- partition xsRef lo hi
            auxSort lo (mid-1)
            auxSort (mid+1) hi

partition :: Ord a => STRef s [a] -> Int -> Int -> ST s Int
partition xs lo hi = do
    pivot <- (!! hi) <$> yield xs
    i <- ref lo

    forM_ [lo..hi] $ \j -> do
        xj <- (!! j) <$> yield xs
        when (xj <= pivot) $ do
            i'  <- yield i
            xs' <- yield xs
            xs =. swap xs' j i'
            i $= succ

    i $= pred
    yield i

