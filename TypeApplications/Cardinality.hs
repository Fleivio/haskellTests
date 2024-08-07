{-# LANGUAGE AllowAmbiguousTypes, TypeApplications#-}
import Data.Void

class Cardinal a where
    cardinality :: Integer

instance (Cardinal a, Cardinal b) => Cardinal (Either a b) where
    cardinality = cardinality @a + cardinality @b

instance (Cardinal a, Cardinal b) => Cardinal (a, b) where
    cardinality = cardinality @a * cardinality @b

instance (Cardinal a, Cardinal b) => Cardinal (a -> b) where
    cardinality = cardinality @b ^ cardinality @a

instance (Cardinal a) => Cardinal (Maybe a) where
    cardinality = succ $ cardinality @a

instance Cardinal Void where
    cardinality = 0

instance Cardinal () where
    cardinality = 1

instance Cardinal Bool where
    cardinality = 2

instance Cardinal Ordering where
    cardinality = 3

instance Cardinal Int where
    cardinality = 2 ^ 64


areIsomorphic :: forall a b. (Cardinal a, Cardinal b) => Bool
areIsomorphic = cardinality @a == cardinality @b

---------------------------------------------------------

-- 1 * a = a
prodUnitTo :: a -> (a,())
prodUnitTo x = (x,())

prodUnitFrom :: (a,()) -> a
prodUnitFrom (x,()) = x

-- a^b * a^c = a^(b+c)
expRuleTo :: (b -> a, c -> a) -> (Either b c -> a)
expRuleTo (f1, _ ) (Left b)  = f1 b
expRuleTo (_ , f2) (Right c) = f2 c

expRuleFrom :: (Either b c -> a) -> (b -> a, c -> a)
expRuleFrom f = (f . Left, f . Right)

-- (a * b)^c = a^c * b^c
distRuleTo :: (c -> (a,b)) -> (c -> a, c -> b)
distRuleTo f = (fst . f, snd . f)

distRuleFrom :: (c -> a, c -> b) -> (c -> (a,b))
distRuleFrom (f, g) = \c -> (f c, g c)

-- (a^b)^c = a^(b*c)
multpowRuleTo :: (c -> b -> a) -> ((c,b) -> a)
multpowRuleTo = uncurry

multpowRuleFrom :: ((c,b) -> a) -> (c -> b -> a)
multpowRuleFrom = curry

--TicTacToe-----------------------------

data Mark = X | O deriving (Eq, Show)

instance Cardinal Mark where
    cardinality = 2

type TicTacToe a = a -> a -> Maybe Mark

emptyBoard :: TicTacToe a
emptyBoard = const $ const Nothing

markAt :: Eq a => Mark -> (a,a) -> TicTacToe a -> TicTacToe a
markAt mark (x,y) tab x1 y1 = if (x1,y1) == (x,y) then Just mark else tab x1 y1

instance Cardinal a => Cardinal (TicTacToe a) where
    cardinality = (cardinality @(Maybe Mark)) ^ (cardinality @a * cardinality @a)

type TicTacToe3 = TicTacToe Ordering

instance (Bounded a, Enum a) => Show (TicTacToe a) where
    show t = unlines [unwords [show $ t x y | x <- [minBound .. maxBound]] | y <- [minBound .. maxBound]]
