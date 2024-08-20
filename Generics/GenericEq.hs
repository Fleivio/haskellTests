{-#LANGUAGE DeriveGeneric, DeriveAnyClass, DefaultSignatures, TypeOperators, StandaloneDeriving, FlexibleContexts #-}

import GHC.Generics

class GEq a where
  geq :: a x -> a x -> Bool

instance GEq V1 where
  geq _ _ = True

instance GEq U1 where
  geq U1 U1 = True

instance Eq a => GEq (K1 _1 a) where
  geq (K1 a) (K1 b) = a == b

instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 a1) (L1 a2) = geq a1 a2
  geq (R1 a1) (R1 a2) = geq a1 a2
  geq _       _       = False

instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: a2) (b1 :*: b2) = geq a1 b1 && geq a2 b2

instance (GEq a) => GEq (M1 _x _y a) where
  geq (M1 a1) (M1 a2) = geq a1 a2

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = from a `geq` from b

class MyEq a where
  eq :: a -> a -> Bool

  default eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
  eq = genericEq

data Foo a b c = F0 | F1 a | F2 b c 
  deriving (Generic, MyEq)
