import           Data.Functor.Const
import           Data.Functor.Identity

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter = \wrapper s -> setter s <$> wrapper (getter s)


--------------------------------------------------------
view :: Lens s t a a -> s -> a
view len s = getConst $ len Const s

over :: Lens s t a b -> (a -> b) -> s -> t
over len f s = runIdentity $ len (Identity . f) s

set :: Lens s t a b -> b -> s -> t
set len b s = runIdentity $ len (const $ Identity b) s

(^.) = view

(.~) = set

(%~) = over

(&) :: a -> (a -> b) -> b
a & f = f a


--EXAMPLE-----------------------------------------------------
data Point = Point
  { _x :: Double
  , _y :: Double
  } deriving (Eq, Show)

x :: Lens' Point Double
x = lens _x (\s a -> s {_x = a})

y :: Lens' Point Double
y = lens _y (\s a -> s {_y = a})

newtype Transform = Transform
  { _point :: Point
  } deriving (Eq, Show)

point :: Lens' Transform Point
point = lens _point (\t p -> t {_point = p})


--Tuples----------------------------------------------------------
_1 :: Lens' (a, b) a
_1 = lens fst (\(_, b) a -> (a, b))
