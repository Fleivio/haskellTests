{-#LANGUAGE AllowAmbiguousTypes, TypeApplications#-}
module TypeApp where 
apply :: forall a b. (a -> b) -> a -> b
apply f a = b
    where
        b = f a

test :: Printable a => a -> String
test (b::a) = showType @a

class Printable a where
    showType :: String

instance Printable Int where
    showType = "Int"

instance Printable Char where
    showType = "Char"

instance Printable a => Printable [a] where
    showType = "[" ++ showType @a ++ "]"