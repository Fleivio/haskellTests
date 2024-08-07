
-- Isomorphic to Function
data Reader w a = Reader {runReader :: w -> a}

instance Functor (Reader w) where
    fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader w) where
    pure a = Reader $ const a

    r1 <*> r2 = Reader $ \w -> runReader r1 w $ runReader r2 w

instance Monad (Reader w) where
    return = pure

    r1 >>= f = 
        Reader $ \w ->
            let a = runReader r1 w
            in runReader (f a) w

ask :: Reader w w
ask = Reader id

asks :: (w -> x) -> Reader w x
asks = Reader

local :: (w -> w) -> Reader w a -> Reader w a
local f (Reader g) = Reader $ g . f

----------------------------------------

data Environtment = Env {
        temperature :: Double,
        pressure :: Double,
        umidity :: Double
    } deriving (Eq, Show)

runEnv :: Reader Environtment String 
runEnv = do
    t <- asks temperature
    if t <= 0 then return "cold"
    else return "maybe hot"