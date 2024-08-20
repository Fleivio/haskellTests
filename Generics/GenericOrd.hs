{-#LANGUAGE DeriveGeneric, DeriveAnyClass, DefaultSignatures, TypeOperators, StandaloneDeriving, FlexibleContexts #-}

import GHC.Generics

class GOrd a where
  gcompare :: a x -> a x -> Ordering

instance GOrd V1 where
  gcompare _ _ = EQ

instance GOrd U1 where
  gcompare U1 U1 = EQ

instance Ord x => GOrd (K1 _1 x) where
  gcompare (K1 a) (K1 b) = compare a b

instance (GOrd a, GOrd b) => GOrd (a :+: b) where
  gcompare (L1 a) (L1 b) = gcompare a b
  gcompare (R1 a) (R1 b) = gcompare a b
  gcompare (L1 _) (R1 _) = GT 
  gcompare _      _      = LT

instance (GOrd a, GOrd b) => GOrd (a :*: b) where
  gcompare (a1 :*: b1) (a2 :*: b2) = gcompare a1 a2 <> gcompare b1 b2

instance (GOrd a) => GOrd (M1 _x _y a) where
  gcompare (M1 a) (M1 b) = gcompare a b

genericOrd :: (GOrd (Rep a), Generic a) => a -> a -> Ordering
genericOrd x y = from x `gcompare` from y

