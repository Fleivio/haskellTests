
import Data.List ( partition )

data Prob a = 
    Prob [(a, Double)]
    deriving (Eq, Show)

unProb :: Prob a -> [(a, Double)]
unProb (Prob x) = x

fstApp :: (a -> c) -> (a, b) ->  (c, b)
fstApp f (a,b) = (f a, b)

instance Functor Prob where
    fmap f (Prob xs) = Prob (fstApp f <$> xs)

instance Applicative Prob where
    pure x = Prob [(x,1)]

    Prob fs <*> Prob xs = 
        Prob $ do 
            (f, fp) <- fs
            (x, xp) <- xs
            return (f x, fp * xp)

instance Monad Prob where
    return = pure

    Prob xs >>= f =
        Prob $ do
            (x, xp) <- xs
            (y, yp) <- unProb $ f x
            return (y, yp * xp)

uniform :: [a] -> Prob a
uniform xs = Prob [ (x', 1/fromIntegral (length xs)) | x' <- xs] 

data Face = Coroa | Cara deriving (Eq, Show)

coin :: Prob Face
coin = uniform [Coroa, Cara]

doubleCoin :: Prob (Face, Face)
doubleCoin = (,) <$> coin <*> coin

doubleCoin' :: Prob (Face, Face)
doubleCoin' = do
    x1 <- coin
    x2 <- coin 
    return (x1, x2)

--------------------------------------

medicalTest :: Bool -> Prob Bool
medicalTest True = Prob [(True, 0.95), (False, 0.05)]  
medicalTest False = Prob [(True, 0.05), (False, 0.95)]  

test = do
    hasDisease <- Prob [(True, 0.01), (False, 0.99)]
    testPositive <- medicalTest hasDisease
    return (hasDisease, testPositive)

