{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind

type HetList :: [Type] -> Type

data HetList ts where
  HNil :: HetList '[]
  (:>) :: a -> HetList ts -> HetList (a : ts)

infixr 9 :>

hHead :: HetList (t : ts) -> t
hHead (t :> _) = t

toTuple :: HetList '[ a, b] -> (a, b)
toTuple (a :> b :> HNil) = (a, b)

toTriple :: HetList '[ a, b, c] -> (a, b, c)
toTriple (a :> b :> c :> HNil) = (a, b, c)


-- Abordagem 1
instance Show (HetList '[])
 where
  show _ = ""

instance (Show a, Show (HetList as)) => Show (HetList (a : as))
 where
  show (a :> as) = show a ++ " :> " ++ show as


-- Abordagem 2
class Show2 a where
  show2 :: a -> String

type All :: (Type -> Constraint) -> [Type] -> Constraint

type family All c ts
 where
  All _ '[]      = ()
  All c (t : ts) = (c t, All c ts)

instance All Show ts => Show2 (HetList ts)
 where
  show2 (HNil)    = ""
  show2 (a :> as) = show a ++ " :-) " ++ show2 as

instance All Eq ts => Eq (HetList ts)
 where
  HNil == HNil       = True
  a :> as == b :> bs = a == b && as == bs
