
import Prelude hiding (Traversable (..))

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

    sequence :: {-Monad f-} Applicative f => t (f a) -> f (t a)

instance Traversable [] where
    traverse f []     = pure []
    traverse f (x:xs) = (:) <$> f x <*> traverse f xs
    
    sequence = traverse id

instance Traversable Maybe where
    traverse f Nothing = pure Nothing
    traverse f (Just a) = Just <$> f a

    sequence = traverse id
