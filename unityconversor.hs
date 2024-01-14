{-#LANGUAGE RankNTypes, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes,
 MultiParamTypeClasses, FlexibleInstances, LambdaCase #-}

concatena :: 
    (Show b, Show c) => (forall a. Show a => a -> String) -> b -> c -> String
concatena f b c = f b ++ f c

foo :: String
foo = concatena (\x -> show x) 1 2

class Tipavel a where
    nomeTipo :: a -> String
    nomeTipo _ = nomeTipo0 @a

    nomeTipo0 :: String

instance Tipavel Int where
    nomeTipo0 = "Int"

instance Tipavel Float where
    nomeTipo0 = "Float"

data Grama
data Kilograma
data Onça
data Libra
data Pedra

class Unidade u where
    sigla :: String
    fator :: Floating a => a

instance Unidade Grama where
    sigla = "g"
    fator = 1

instance Unidade Kilograma where
    sigla = "kg"
    fator  = 1000

instance Unidade Onça where
    sigla = "oz"
    fator = 28.35

instance Unidade Libra where
    sigla = "lb"
    fator = 453.59

instance Unidade Pedra where
    sigla = "st"
    fator = 14 * fator @Libra

class Conv a b where 
    conv :: Floating f => f -> f

instance (Unidade a, Unidade b) => Conv a b where
    conv x = x * fator @a / fator @b

data Proxy a = Proxy

-- unToType :: String -> Proxy u
-- unToType = \case 
--     "g"  -> Proxy :: Proxy Grama
--     "kg" -> Proxy :: Proxy Kilograma
--     "oz" -> Proxy :: Proxy Onça
--     "lb" -> Proxy :: Proxy Libra
--     "st" -> Proxy :: Proxy Pedra