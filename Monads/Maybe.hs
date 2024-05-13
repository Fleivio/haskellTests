
import Prelude hiding (Maybe(..))

data Maybe a = Just a | Nothing deriving (Eq, Show)

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just $ f a

instance Applicative Maybe where
    pure = Just

    Just f <*> Just x = Just $ f x
    _      <*> _      = Nothing

instance Monad Maybe where
    return = pure 

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    m >>= f = case f <$> m of
                Just a -> a
                Nothing -> Nothing 
