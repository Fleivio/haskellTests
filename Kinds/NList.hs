{-#LANGUAGE DataKinds, ConstraintKinds, TypeApplications, GADTs, AllowAmbiguousTypes,
 TypeFamilies, UndecidableInstances, TypeFamilies #-}

import Data.Kind 
import GHC.TypeLits
import Data.Type.Equality
import Unsafe.Coerce
import Language.Haskell.TH (Overlap(Incoherent), Extension (OverlappingInstances, IncoherentInstances))

type NList :: Type -> Natural -> Type
data NList a s where
  NNil :: NList a 0
  (:>) :: a -> NList a s -> NList a (s + 1)
infixr 3 :>

instance Show a => Show (NList a s) where
  show NNil = "#" 
  show (a :> as) = show a ++ ":" ++ show as

revPlusProof :: forall n n0. n0 ~ (n - 1) => n0 + 1 :~: n
revPlusProof = unsafeCoerce Refl

lteConvincer :: forall n n0. (n <= n0) :~: (1 <= 1)
lteConvincer = unsafeCoerce Refl

class Base a where
  base :: [a]

instance Base Bool where

  base = [True, False]

instance {-# OVERLAPPING #-} (Base a) => Base (NList a 0) where
  base = [NNil]

instance (Base a, Base (NList a (s-1))) => Base (NList a s) where
  base = case revPlusProof @s @(s-1) of
           Refl -> [ a :> as | a <- base @a, as <- base @(NList a (s-1)) ]