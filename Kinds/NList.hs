{-#LANGUAGE DataKinds, ConstraintKinds, TypeApplications, GADTs, AllowAmbiguousTypes,
 TypeFamilies, UndecidableInstances#-}

import Data.Kind 
import GHC.TypeLits
import Data.Type.Equality
import Unsafe.Coerce

type NList :: Type -> Natural -> Type
data NList a s where
  NNil :: NList a 0
  (:>) :: a -> NList a s -> NList a (s + 1)
infixr 3 :>

instance Show a => Show (NList a s) where
  show NNil = "#" 
  show (a :> as) = show a ++ ":" ++ show as

class Base a where
  base :: [a]

instance Base Bool where
  base = [True, False]

revPlusProof :: forall n n0. n0 ~ (n - 1) => n0 + 1 :~: n
revPlusProof = unsafeCoerce Refl

lteConvincer :: forall n n0. (n <= n0) :~: (1 <= 1)
lteConvincer = unsafeCoerce Refl

genLists :: [x] -> Proxy size -> NList x size
genLists xs = xs

instance (Base x, n ~ 0) => Base (NList x n) where
  base = [NNil]

instance (Base x, n ~ (n0 + 1)) => Base (NList x n) where
  base = case revPlusProof @n0 @(n0-1) of
           Refl -> [x :> xs | x <- base @x, xs <- base @(NList x n0)]