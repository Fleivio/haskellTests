import           Data.Functor.Const
import           Data.Functor.Identity

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter = \wrapper s -> setter s <$> wrapper (getter s)


--------------------------------------------------------
view :: Lens s t a b -> s -> a
view len s = getConst $ len Const s

over :: Lens s t a b -> (a -> b) -> s -> t
over len f s = runIdentity $ len (Identity . f) s

set :: Lens s t a b -> b -> s -> t
set len b s = runIdentity $ len (const $ Identity b) s

(^.) :: s -> Lens s t a b -> a
s ^. len = view len s
infixr 8 ^.

(.~) = set
infixr 8 .~

(%~) = over
infixr 8 %~

(&) :: a -> (a -> b) -> b
a & f = f a
infixl 7 &

(<>~) :: (Semigroup a) => Lens' s a -> a -> s -> s
lens <>~ a = over lens (<>a) 

--EXAMPLE-----------------------------------------------------
data Vec2 = Vec2
  { _x :: Double
  , _y :: Double
  } deriving (Eq, Show)

x :: Lens' Vec2 Double
x = lens _x (\s a -> s {_x = a})

y :: Lens' Vec2 Double
y = lens _y (\s a -> s {_y = a})

xy :: Lens' Vec2 (Double, Double)
xy = lens (\s -> (_x s, _y s)) (\s (x1, y1) -> Vec2 x1 y1)

magnitude :: Lens' Vec2 Double
magnitude = lens (\s -> _x s `magn` _y s) 
                 (\s m -> s & xy %~ (both $ (*)(m/s^.magnitude)))
  where
    magn x1 y1 = sqrt $ x1**2 + y1**2
    both f (x1,y1) = (f x1, f y1)

newtype Transform = Transform
  { _point :: Vec2
  } deriving (Eq, Show)

point :: Lens' Transform Vec2
point = lens _point (\t p -> t {_point = p})

default (Int)
main1 = do
  let trs = Transform (Vec2 10 12)
  print $ trs ^. point . x
  print $ trs ^. point . magnitude
  print $ trs & point . magnitude .~ 1 --normalização

  let trs2 = trs
              & point.magnitude .~ 1
              & point.x .~ 0
  print trs2

--Tuples----------------------------------------------------------

_1 :: Lens (a, b) (c, b) a c
_1 = lens fst (\(_, b) a -> (a, b))
