{-# LANGUAGE RankNTypes, AllowAmbiguousTypes, ScopedTypeVariables #-}

------------------

showTuple :: (Show a, Show b) => (a, b) -> (forall c. Show c => c -> String) -> String
showTuple (a, b) myshow = myshow a ++ myshow b

test = putStrLn $ showTuple (1,'a') (\s -> " - " ++ show s ++ " \n")

------------------

data Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r) where
    fmap f (Cont g) = Cont $ \cc -> g (cc . f)

testCont :: Cont r Int
testCont = pure 5

testCont2 :: Cont r String
testCont2 = show <$> testCont

instance Applicative (Cont r) where
    pure a = Cont $ \f -> f a

    f <*> x = Cont $ \b -> runCont f (\ab -> runCont (ab <$> x) b)

testCont3 :: Cont r (Int -> String)
testCont3 = Cont $ \f -> f show

testCont4 :: Cont r String
testCont4 = testCont3 <*> testCont

instance Monad (Cont r) where
    return = pure

    Cont g >>= f = Cont $ \cc -> g $ \a -> runCont (f a) cc


{-

do
    x <- Cont f
    y <- Cont g
    return (x <?> y)

Cont f >>= \x ->
    Cont g >>= \y -> 
        return (x <?> y)

Cont f >>= \x ->
    Cont $ \cc -> g (\a -> return (x <?> a) cc)

Cont f >>= \x ->
    Cont $ \cc -> g (\a -> cc (x <?> a))



-}


testCont5 :: Cont r Int
testCont5 = do
    a <- return 1
    b <- return 2
    return $ b + a

{-
return 1 >>= \x ->
    return 2 >>= \y -> 
        return (x + y)

Cont (\f -> f 1) >>= \x -> 
    Cont (\f -> f 2) >>= \y ->
        return (x + y)
    
    -- Cont g >>= f = Cont $ \cc -> g $ \a -> runCont (f a) cc

Cont (\f -> f 1) >>= \x -> 
    Cont $ \cc -> (\f -> f 2) (\a -> runCont ((\y -> return (x + y)) a) cc)

Cont (\f -> f 1) >>= \x -> 
    Cont $ \cc -> ((\a -> runCont ((\y -> return (x + y)) a) cc) 2) 

Cont (\f -> f 1) >>= \x -> 
    Cont $ \cc -> (runCont ((\y -> return (x + y)) 2) cc)) 

Cont (\f -> f 1) >>= \x -> 
    Cont $ \cc -> (runCont (Cont $ \f -> f (x + 2)) cc)

    -- Cont g >>= f = Cont $ \cc -> g $ \a -> runCont (f a) cc

Cont (\f -> f 1) >>= \x -> 
    Cont $ \cc -> cc (x + 2)

Cont $ \cc2 -> (\f -> f 1) (\a -> runCont ((\x -> Cont $ \cc -> cc (x + 2)) a) cc2)

Cont $ \cc2 -> (runCont ((\x -> Cont $ \cc -> cc (x + 2)) 1) cc2)

Cont $ \cc2 -> cc2 (1 + 2)

-}

-----------------------------------

factCPS :: Int -> Cont r Int
factCPS 0 = pure 1
factCPS n = (n *) <$> factCPS (n - 1)

foo :: Cont r Int
foo = do
    x <- factCPS 3
    y <- factCPS 4
    return (x + y)

--------------------------------------------------


applyTo3 :: Cont r Int
applyTo3 = pure 3

test3 :: Cont r Int
test3 = do
    x <- applyTo3
    y <- applyTo3
    return (x + y)


--------------------------------------------------

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \cc ->
  let Cont g = f (\x -> Cont (const (cc x)))
  in g cc


pi :: Double
pi = 3.14159

circArea :: Double -> Double
circArea r = Main.pi * (r ** 2)

main :: IO ()
main = do
    x <- readLn
    putStrLn $ "A=" ++ (show $ circArea x)