{-# LANGUAGE RankNTypes #-}

import Data.Functor.Const
import Data.Functor.Identity

---[Definições]---

data Person = Person {
        pname :: String,
        city :: City
    } deriving(Eq, Show)

data City = City {
        cname :: String,
        point :: Point
    } deriving(Eq, Show)

data Point = Point {
        x :: Double,
        y :: Double
    } deriving(Eq, Show)

type Lens s a t b =
    forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s a s a

get :: Lens s a t b -> s -> a
get l s = getConst $ l Const s

set :: Lens s a t b -> s -> b -> t
set l s a = runIdentity $ l (const (Identity a)) s

over :: Lens s a t b -> (a -> b) -> s -> t
over l f s = runIdentity $ l (Identity . f) s

lens :: (s -> a) -> (s -> b -> t) -> Lens s a t b
lens get' set' transf s = set' s <$> transf (get' s)

---[Lenses]---

cityLens :: Lens' Person City
cityLens = lens city $ \(Person n _) c -> Person n c

pointLens :: Lens' City Point
pointLens = lens point $ \(City n _) p -> City n p

xLens :: Lens' Point Double
xLens = lens x $ \(Point _ y') x' -> Point x' y'

personSetX :: Person -> Double -> Person
personSetX p x' = set (cityLens . pointLens . xLens) p x'
