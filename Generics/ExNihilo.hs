{-#LANGUAGE DeriveGeneric, DeriveAnyClass, DefaultSignatures, TypeOperators, StandaloneDeriving, FlexibleContexts #-}

import GHC.Generics

class GNil a where
  gexNihilo :: Maybe (a x)

instance GNil V1 where
  gexNihilo = Nothing

instance GNil U1 where
  gexNihilo = Just U1

instance GNil (K1 _1 x) where
  gexNihilo = Nothing

instance (GNil a, GNil b) => GNil (a :+: b) where
  gexNihilo = Nothing

instance (GNil a, GNil b) => GNil (a :*: b) where
  gexNihilo = Nothing

instance (GNil a) => GNil (M1 _x _y a) where
  gexNihilo = M1 <$> gexNihilo @a

exNihilo :: forall a. (GNil (Rep a), Generic a) => Maybe a
exNihilo = to <$> gexNihilo

