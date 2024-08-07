
data RState s a = RState {runRState :: s -> (s, a)}

instance Functor (RState s) where 
    fmap f (RState g) = 
        RState $ \s -> 
            let (s1, a) = g s
            in (s1, f a)

instance Applicative (RState s) where
    pure a = RState $ (,a)

    RState f <*> RState x = 
        RState $ \s ->
            let (s1, x') = x s
                (s2, f') = f s
            in (s2, f' x')

instance Monad (RState s) where
    RState x >>= f = 
        RState $ \s -> 
            let (s1, a) = x s2
                (s2, b) = runRState (f a) s
            in (s1, b)

test :: RState String Int
test = do
    a <- RState $ \s -> (s ++ "bizarro", read s)
    b <- RState $ \s -> (s, a * 4)
    return b