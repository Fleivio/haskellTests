{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}
import Data.Void

class Cardinal a where
    cardinality :: Integer

instance (Cardinal a, Cardinal b) => Cardinal (a, b) where
    cardinality = cardinality @a * cardinality @b

instance (Cardinal a, Cardinal b) => Cardinal (Either a b) where
    cardinality = cardinality @a + cardinality @b

instance Cardinal Void where
    cardinality = 0

instance Cardinal () where
    cardinality = 1

instance Cardinal Bool where
    cardinality = 2

instance Cardinal Int where
    cardinality = 2 ^ 64
