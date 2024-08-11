{-#LANGUAGE DataKinds, TypeFamilies, AllowAmbiguousTypes, TypeApplications #-}

import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Distribution.Simple (KnownExtension(AllowAmbiguousTypes, TypeApplications))

data (a :: k1) % (b :: k2)
infixr 5 %

class Printfable a where
  type Printf a :: Type
  format :: String -> Printf a

instance KnownSymbol text => Printfable (text :: Symbol) where
  type Printf text = String
  format s = s <> symbolVal (Proxy @text)

instance (Printfable a, KnownSymbol t) => Printfable ((t :: Symbol) % a) where
  type Printf (t % a) = Printf a
  format s = format @a (s <> symbolVal (Proxy @t))

instance (Printfable a, Show param) => Printfable ((param :: Type) % a) where
  type Printf (param % a) = param -> Printf a
  format s x = format @a (s <> show x)

instance {-#OVERLAPPING#-} (Printfable a) => Printfable (String % a) where
  type Printf (String % a) = String -> Printf a
  format s x = format @a (s <> x)

printf :: forall a. Printfable a => Printf a
printf = format @a ""

main :: IO ()
main = do
  putStrLn $ printf @("Bom Dia, " % String % ", hoje é dia " % Int % "") "Flavio" 11

