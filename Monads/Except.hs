
-- Isomorphic to Either
data Except b a = 
      Except b
    | Success a
    deriving (Eq, Show)

instance Functor (Except b) where
    fmap f (Except b)  = Except b
    fmap f (Success a) = Success $ f a

instance Applicative (Except b) where
    pure = Success

    (Success f) <*> (Success x) = Success $ f x
    (Except h)  <*> _           = Except h
    _           <*> (Except h)  = Except h

instance Monad (Except b) where
    return = pure

    (Except h)  >>= f = Except h
    (Success a) >>= f = f a

trow :: b -> Except b ()
trow = Except

catch :: (b -> Except b a) -> Except b a  -> Except b a
catch _ (Success a) = Success a
catch f (Except b)  = f b

data MathException = Div0 | InvLogBase | InvLogOp deriving (Eq, Show)

safeDiv :: Int -> Int -> Except MathException Int
safeDiv a 0 = Except Div0
safeDiv a b = Success $ a `div` b

safeLog :: Int -> Int -> Except MathException Int 
safeLog b l
    | b == 0 || b == 1 = Except InvLogBase
    | l <= 0 = Except InvLogOp
    | otherwise = Success $ round $ logBase (fromIntegral b) (fromIntegral l)

test = catch (\e -> case e of
                 InvLogBase -> Success 1
                 Div0 -> Success 0
                 _ -> Except e) $
        do
        a <- safeDiv 1 0
        safeLog 0 a
    