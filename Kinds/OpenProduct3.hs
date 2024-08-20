{-#LANGUAGE GADTs, DataKinds, TypeApplications #-}

import Unsafe.Coerce
import GHC.TypeLits
import Data.Kind

data Forgotten where
  Forget :: a -> Forgotten

remember :: forall a. Forgotten -> a
remember (Forget a) = unsafeCoerce a

--------------------------------------

type Open :: [(Symbol, t)] -> Type
data Open ts where
  Open :: [Forgotten] -> Open ts

