module Imp((=:), ($=), (+=), (-=), (\=), (*=), liftST, while) where

import Control.Monad.ST
import Control.Monad
import Data.STRef

-----[Definições]------------------------------------------

run = runST
ref = newSTRef
yield = readSTRef

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

whileTest :: ST s Integer
whileTest = do
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
            isEven <- even <$> yield actual
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


main = print ()