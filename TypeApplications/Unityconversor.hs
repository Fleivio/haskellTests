{-#LANGUAGE TypeApplications,  AllowAmbiguousTypes#-}

data Grama
data Kilograma
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