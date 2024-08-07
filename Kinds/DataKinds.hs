{-#LANGUAGE DataKinds, TypeApplications, TypeFamilies, GADTs, ConstraintKinds, FunctionalDependencies #-}

import Data.Kind
import Data.Maybe
import GHC.TypeLits

-- funçoes em nivel de tipo
type Or :: Bool -> Bool -> Bool 
type family Or x y where
  Or 'True y = 'True
  Or 'False y = y

type Not :: Bool -> Bool 
type family Not x where
  Not 'True  = 'False 
  Not 'False = 'True

type All :: [Bool] -> Bool
type family All x where
  All ('True ': xs) = All xs
  All ('False ': _) = 'False
  All '[] = 'True

type User :: [Bool] -> Type
data User permissions where
  Admin :: String -> User '[ 'True, 'True]
  User :: String -> User '[ 'True, 'False]
deriving instance Show (User pr)

data ConnectionStatus = Open | Closed

type Connection :: ConnectionStatus -> Type
data Connection st where
  MkConnection :: String -> Connection Closed
  OpenConnection :: (All pr ~ 'True) => User pr -> String -> Connection Open

openConnection :: (All pr ~ 'True) => User pr -> Connection Closed -> Connection Open
openConnection u (MkConnection name) = OpenConnection u name

              {- (~) representa igualdade entre tipos -}
tryAccess :: All pr ~ 'True => User pr -> IO ()
tryAccess _ = print "adm"

main :: IO ()
main = do 
  tryAccess (Admin "Carlos")
  -- tryAccess (User "Luiz") -- erro em tempo de compilação, Luiz não tem todas as permissões necessárias

-------------------------------

type NonEmpty :: (Type -> Type) -> Type -> Type
data NonEmpty f a = LNonEmpty a (f a)

------------------------------

type Money :: Symbol -> Type
newtype Money s = Money Rational 

doisReais :: Money "R$"
doisReais = Money 2

doisEuros :: Money "EUR"
doisEuros = Money 4

-- nao é possivel somar moedas diferentes
instance Num (Money c) where
  Money a + Money b = Money $ a+b
