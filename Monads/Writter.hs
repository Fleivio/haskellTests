
data Writer w a = Writer (a, w) deriving (Show)

runWriter :: Writer w a -> (a, w)
runWriter (Writer k) = k

instance Functor (Writer w) where
    fmap f (Writer (a, w)) = Writer (f a, w) 

instance Monoid w => Applicative (Writer w) where
    pure a = Writer $ (a, mempty)

    Writer (f, w1) <*> Writer (x, w2) 
        = Writer (f x, w1 <> w2)

instance Monoid w => Monad (Writer w) where
    return = pure

    Writer (x, w) >>= f = 
        Writer $ let (Writer (y, w1)) = f x
                 in (y, w1 <> w)

tell :: w -> Writer w ()
tell w = Writer ((), w)

listen :: Writer w a -> Writer w (a, w)
listen (Writer (a, w)) = Writer ((a,w), w)

type Logger a = Writer String a

double :: Int -> Logger Int
double a = do 
    tell "*2"
    return $ a * 2

suc :: Int -> Logger Int
suc a = do
    tell "succ"
    return $ a + 1

prev :: Int -> Logger Int
prev a = do
    tell "prev"
    return $ a - 1

test :: Logger Int
test = do
    a <- suc 4
    b <- prev a
    double b