
data Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity

    Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
    return = pure

    Identity a >>= f = f a

test :: Int -> Identity Int
test = do
    x <- pure . succ
    return x