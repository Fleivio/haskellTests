{-#LANGUAGE Rank2Types#-}

import Data.Functor.Const
import Data.Functor.Identity

type Lens' s a = 
    forall f. Functor f =>
        (a -> f a) -> s -> f s

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens getter setter wrapper s = 
    setter s <$> wrapper (getter s)

view :: Lens' s a -> s -> a
view len s = getConst $ len Const s

set :: Lens' s a -> s -> a -> s
set len s b = runIdentity $ len (const $ Identity b) s

over :: Lens' s a -> s -> (a -> a) -> s
over len s f = runIdentity $ len (Identity . f) s

(^.) :: s -> Lens' s a -> a
a ^. s = view s a

---------------------------------------------------

data Point = Point {
    _x :: Int,
    y :: Int
} deriving Show

x :: Lens' Point Int
x = lens _x (\p b -> p{_x = b})

data Transform = Transform {
    _position :: Point,
    speed :: Point
} deriving Show

position :: Lens' Transform Point
position = lens _position (\t p -> t{_position = p}) 

data Character = Character {
    _transform :: Transform,
    name :: String
} deriving Show

transform :: Lens' Character Transform
transform  = lens _transform (\c t -> c{_transform = t})

--------------------------------------------------

translateChar :: Character -> Character
translateChar s = over (transform.position.x) s (+1)

setToOrigin :: Character -> Character
setToOrigin s = set (transform.position.x) s 0

------------------------------------
