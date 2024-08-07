
data Tardis s1 s2 a = Tardis {runTardis :: s1 -> s2 -> (s1, s2, a)}

instance Functor (Tardis s1 s2) where
    fmap f (Tardis g) = Tardis $ \s1 s2 ->
        let (s11, s22, x) = g s1 s2
        in (s11, s22, f x)

instance Applicative (Tardis s1 s2) where
    pure x = Tardis (,,x)

    Tardis f <*> Tardis x =
        Tardis $ \s1 s2 ->
            let (s11, s222, f') = f s1 s22
                (s111, s22, x') = x s11 s2
            in (s111, s222, f' x')

instance Monad (Tardis s1 s2) where
    Tardis a >>= f = 
        Tardis $ \s1 s2 -> 
            let (s11, s222, a') = a s1 s22
                (s111, s22, k) = runTardis (f a') s11 s2
            in (s111, s222, k)

test :: Tardis String String ()
test = do
    Tardis (\s1 s2 -> (s1 ++ "1", s2 ++ "1" , ()))
    Tardis (\s1 s2 -> (s1 ++ "2", s2 ++ "2" , ()))

-------------------------------------------

data Color a = Color a a a deriving (Eq, Show, Functor)
type Color' = Color (Sum Float)

color :: Float -> Float -> Float -> Color'
color a b c = Color (Sum a) (Sum b) (Sum c)

blue :: Color'
blue = color 0 0 1

red :: Color'
red = color 1 0 0

green :: Color'
green = color 0 1 0

black :: Color'
black = color 0 0 0

data Sum a = Sum a deriving (Eq, Show, Functor)

instance Semigroup a => Semigroup (Color a) where
    (Color a b c) <> (Color a1 b1 c1) = Color (a <> a1) (b <> b1) (c <> c1)

instance (Num a) => Semigroup (Sum a) where
    Sum b <> Sum a = Sum $ a + b

data Surface = Surface Color' Float

appSurface :: Surface -> Tardis Int Color' ()
appSurface (Surface c1 d) = 
    Tardis $ \depth col ->
        (depth + 1, ((fmap (*(d * recip (fromIntegral depth)))) <$> col) <> c1, ())

raytracing :: Tardis Int Color' ()
raytracing = do
    appSurface (Surface blue 0.5)
    appSurface (Surface red 0.9)
