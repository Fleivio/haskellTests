-- Isomorphic to Except
import           Prelude hiding (Either (..))

data Either a b
  = Left a
  | Right b
  deriving (Eq, Show)

instance Functor (Either a)
 where
  fmap f (Left a)  = Left a
  fmap f (Right b) = Right $ f b

instance Applicative (Either a)
 where
  pure = Right
  (<*>) :: Either a1 (a2 -> b) -> Either a1 a2 -> Either a1 b
  Right f <*> Right b = Right $ f b
  Left a <*> _        = Left a
  _ <*> Left a        = Left a

instance Monad (Either a)
 where
  return        = pure
  Right b >>= f = f b
  Left a >>= _  = Left a
