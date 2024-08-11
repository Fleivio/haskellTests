{-#Language GADTs, DataKinds, ConstraintKinds, UndecidableInstances#-}

import Data.Typeable
import Data.Kind

type Has :: (Type -> Constraint) -> Type
data Has c where
  Has :: c a => a -> Has c

eliminator :: (forall a. c a => a -> r) -> Has c -> r
eliminator c (Has a) = c a

instance Show (Has Show) where
  show (Has a) = show a

instance Show (Has ShowEq) where
  show (Has a) = show a

-------------------------

class (Show a, Eq a) => ShowEq a -- se "type" cria um sinonimo de tipo, class cria um sinonimo de contraint
instance (Show a, Eq a) => ShowEq a 
