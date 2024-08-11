{-#LANGUAGE GADTs#-}

import Data.Typeable -- impede type-erasure
import Data.Maybe
import Control.Applicative

default (Int)

-- Equivalente a Has Typeable
data Dyn where
  Dyn :: (Typeable a) => a -> Dyn

eliminator :: (forall a. Typeable a => a -> r) -> Dyn -> r
eliminator f (Dyn a) = f a

fromDyn :: Typeable a => Dyn -> Maybe a
fromDyn = eliminator cast

liftDyn :: forall a b c.
    (Typeable a, Typeable b, Typeable c) =>
    Dyn -> Dyn -> (a -> b -> c) -> Maybe Dyn
liftDyn d1 d2 f = do
    j <- fromDyn @a d1
    k <- fromDyn @b d2
    return (Dyn $ f j k)

(+++) :: Dyn -> Dyn -> Maybe Dyn
b +++ a = asum [
        liftDyn @String @String a b (++)
    ,   liftDyn @Int @Int a b (+)
    ,   liftDyn @Int @String a b (\a' b' -> show a' ++ b')
    ,   liftDyn @String @Int a b (\a' b' -> a' ++ show b')
    ]

instance Show Dyn where
  show a = fromMaybe (error "no printable") $ asum [
            fromDyn @String a,
            show <$> fromDyn @Int a
          ]