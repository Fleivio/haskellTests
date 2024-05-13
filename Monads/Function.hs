
-- Isomorphic to Reader
data Function a b = Function {runF :: a -> b}

instance Functor (Function a) where
    fmap f (Function g) = Function $ f . g

instance Applicative (Function a) where
    pure x = Function $ const x

    f <*> g = Function $ \x -> runF f x $ runF g x

instance Monad (Function a) where
    return = pure

    f >>= g = 
        Function $ \x ->
            let a = runF f x
            in runF (g a) x

ask :: Function a a 
ask = Function id

asks :: (w -> x) -> Function w x
asks = Function

local :: (a -> a) -> Function a b -> Function a b
local f (Function g) = Function $ g . f

----------------------------------------

data Environtment = Env {
        temperature :: Double,
        pressure :: Double,
        umidity :: Double
    } deriving (Eq, Show)

runEnv :: Function Environtment String 
runEnv = do
    t <- asks temperature
    if t <= 0 then return "cold"
    else return "maybe hot"

--------------------------------------------

incTemp :: Environtment -> Environtment
incTemp = (\(Env t p u) -> Env (t + 1) p u)

runEnv' :: Environtment -> String
runEnv' = do
    incTemp 
    t <- temperature
    if t <= 0 then return "cold"
    else return "maybe hot"
