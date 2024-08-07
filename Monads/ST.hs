
import Control.Monad.ST
import Control.Monad
import Data.STRef

-----[Definições]------------------------------------------

run = runST
ref = newSTRef
yield = readSTRef

(<$$>) :: (a -> b) -> STRef s a -> ST s b
f <$$> rf = f <$> yield rf

(=.) :: STRef s a -> a -> ST s ()
(=.) = writeSTRef

(=:) :: STRef s a -> STRef s a -> ST s ()
a =: b = do
    b' <- yield b
    writeSTRef a b'

($=) :: STRef s a -> (a -> a) -> ST s ()
($=) = modifySTRef

(+=) :: (Num a) => STRef s a -> a -> ST s ()
ref += a = ref $= (+a)

(\=) :: (Fractional a) => STRef s a -> a -> ST s ()
ref \= a = ref $= (/a)

(-=) :: (Num a) => STRef s a -> a -> ST s ()
ref -= a = ref $= subtract a

(*=) :: (Num a) => STRef s a -> a -> ST s ()
ref *= a = modifySTRef ref (*a)

liftST :: (t2 -> t1 -> b) -> STRef s t2 -> STRef s t1 -> ST s b
liftST f a b = do
    a <- readSTRef a
    b <- readSTRef b
    return $ f a b

while :: ST s Bool -> ST s () -> ST s ()
while condition arg = do
    c <- condition
    when c $ arg >> while condition arg

------[Testes]--------------------------------------------------

iterTest :: Int
iterTest = run $ do
    a <- ref 2
    b <- ref 0
    b =: a  -- b = a
    a <- ref 1
    a *= 3  -- a = a * 3
    yield b -- return b

whileTest :: Integer
whileTest = run $ do
    a <- ref 0
    b <- ref 10
    while (liftST (<) a b) $ do
        a += 1
    yield a

forTest :: [Int]
forTest = run $ do
    a <- ref 1
    forM [0..10] $ \i -> do
        a += i
        yield a

factorial :: (Integral a) => a -> a
factorial x = case x of
    0 -> 1
    1 -> 1
    _ -> run $ do
        acc <- ref 1

        forM_ [1..x] $ \i -> do
            acc *= i

        yield acc

isPrime :: (Integral a) => a -> Bool
isPrime 1 = False
isPrime x = run $ do
    numDiv <- ref 0
    let upperB = sqrt (fromIntegral x :: Double)

    forM_ [1..upperB]
        ( \i -> do
            when (x `mod` round i == 0)
                (
                    do numDiv += 1
                )
        )

    a' <- ref 1
    liftST (==) numDiv a'

collatz :: Int -> Int
collatz n = run $ do
    iters  <- ref 0
    actual <- ref n

    limit <- ref 1
    while (liftST (/=) actual limit)
        (do
            iters += 1
            isEven <- even <$$> actual
            if isEven
            then actual $= flip div 2
            else actual *= 3 >> actual += 1
        )
    yield iters

dumbMultiply :: Int -> Int -> Int
dumbMultiply n m
    | m < 0 = error "=("
    | otherwise = run $ do
    count <- ref 0
    maxBound <- ref m
    acc <- ref 0

    while (liftST (/=) count maxBound) $ do
        count += 1
        acc += n
    yield acc


stPart :: [a] -> (a -> Bool) -> ([a], [a])
stPart xs f = run $ do
    left  <- ref []
    right <- ref []

    forM_ (reverse xs) $ \i ->
        if f i
        then left  $= (:)i
        else right $= (:)i

    l <- yield left
    r <- yield right
    return (l, r)

---------------------------------------------------

swap :: [a] -> Int -> Int -> [a]
swap xs i' j' = [ if i == i' then xs !! j' else
                  if i == j' then xs !! i' else
                   x | (i, x) <- zip [0..] xs]

writeAt :: [a] -> Int -> a -> [a]
writeAt [] _ _ = []
writeAt (_:xs) 0 x = x : xs
writeAt (y:xs) n x = y : writeAt xs (n-1) x

quickSort :: (Ord a) => STRef s [a] -> ST s ()
quickSort xsRef = do
    len <- length <$$> xsRef
    auxSort 0 (len - 1)
    where
    auxSort lo hi =
        when (lo < hi) $ do
            mid <- partition xsRef lo hi
            auxSort lo (mid-1)
            auxSort (mid+1) hi

partition :: Ord a => STRef s [a] -> Int -> Int -> ST s Int
partition xs lo hi = do
    pivot <- (!! hi) <$$> xs
    i <- ref lo

    forM_ [lo..hi] $ \j -> do
        xj <- (!! j) <$$> xs
        when (xj <= pivot) $ do
            i'  <- yield i
            xs' <- yield xs
            xs =. swap xs' j i'
            i $= succ

    i $= pred 
    yield i

bubbleSort :: Ord a => STRef s [a] -> ST s ()
bubbleSort arr = do
    leng <- length <$$> arr

    forM_ [0..leng-1] $ \i -> do
        forM_ [i+1..leng-1] $ \j -> do
            ai <- (!!i) <$$> arr
            aj <- (!!j) <$$> arr
            when (ai > aj) $ do
                arr' <- yield arr
                arr =. swap arr' i j

radixSort :: STRef s [Int] -> ST s ()
radixSort arr = do
    let max = 1000
        calcRad ai exponent = ai `div` exponent `mod` 10
    leng <- length <$$> arr
    aux  <- ref $ replicate leng 0
    buck <- ref $ replicate 10 0

    forM_ (takeWhile (<=max) $ iterate (*10) 1) $ \exponent -> do

        forM_ [0..leng-1] $ \i -> do
            ai <- (!!i) <$$> arr
            let rad = calcRad ai exponent
            previousVal <- (!!rad) <$$> buck
            buck $= \a -> writeAt a rad (previousVal+1)

        buck $= scanl1 (+)

        forM_ (reverse [0..leng-1]) $ \i -> do
            ai <- (!!i) <$$> arr
            let rad = calcRad ai exponent
            val <- (!!rad) <$$> buck
            aux $= \a -> writeAt a (val-1) ai
            buck $= \a -> writeAt a rad (val-1)

        arr =: aux
        buck =. replicate 10 0

merging :: (Ord a, Show a) => STRef s [a] -> Int -> Int -> Int -> ST s ()
merging arr lo mid hi = do
    aux <- ref []

    l <- ref lo
    r <- ref mid

    forM_ [lo..hi-1] $ \_ -> do
        l' <- yield l
        r' <- yield r

        let lGEmid = l' >= mid
            rGEhi  = r' >= hi
        
        if not lGEmid && not rGEhi then do
            al <- (!!l') <$$> arr
            ar <- (!!r') <$$> arr
            if al < ar then do 
                l   $= succ
                aux $= (++[al])
            else do
                r   $= succ
                aux $= (++[ar])
        else 
            if lGEmid then do
                ar <- (!!r') <$$> arr
                r   $= succ
                aux $= (++[ar])
            else do
                al <- (!!l') <$$> arr
                l   $= succ
                aux $= (++[al])

    forM_ [lo..hi-1] $ \i -> do
        l <- head <$$> aux
        arr $= (\a -> writeAt a i l)
        aux $= tail

mergeSort :: (Ord a, Show a) => STRef s [a] -> ST s ()
mergeSort arr = do
    l <- length <$$> arr
    runMerge 0 l
    where 
        runMerge lo hi 
            | hi - lo <= 1  = return ()
            | otherwise = do
            let mid = (lo + hi) `div` 2
            runMerge lo mid
            runMerge mid hi
            merging arr lo mid hi 

main :: IO ()
main = do
    let xs = reverse [1..10]
    print $ run $ do
        xsRef <- ref xs
        mergeSort xsRef
        yield xsRef