{-#LANGUAGE DataKinds, PolyKinds, TypeFamilies, AllowAmbiguousTypes, UndecidableInstances#-}

import Data.Kind

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Fst :: (a,b) -> Exp a
data Snd :: (a,b) -> Exp b
data FromMaybe :: a -> Maybe a -> Exp a
data LTM :: [a] -> Exp (Maybe a)
data MapList :: (a -> Exp b) -> [a] -> Exp [b]
data FoldR :: (a -> b -> Exp b) -> b -> [a] -> Exp b 
data Pure :: a -> Exp a
data PureF :: (a -> b) -> a -> Exp b
data TypEq :: a -> b -> Exp Bool

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
infixr 0 =<<
data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c 
infixr 1 <=<

data Collapse :: [Constraint] -> Exp Constraint
type All (c :: k -> Constraint) (ts :: [k]) = Collapse =<< MapList (PureF c) ts

data Map :: (a -> Exp b) -> f a -> Exp (f b)
data Mappend :: a -> a -> Exp a
data Mempty :: a -> Exp a
 
type instance Eval (Fst '(a,b)) = a
type instance Eval (Snd '(a,b)) = b
type instance Eval (FromMaybe _ (Just b)) = b
type instance Eval (FromMaybe a Nothing) = a
type instance Eval (LTM '[]) = Nothing
type instance Eval (LTM (x:xs)) = Just x
type instance Eval (MapList f '[]) = '[]
type instance Eval (MapList f (x:xs)) = Eval (f x) : Eval (MapList f xs)

type instance Eval (FoldR f i '[]) = i
type instance Eval (FoldR f i (x:xs)) = Eval (FoldR f (Eval (f x i)) xs)

type instance Eval (Pure a) = a
type instance Eval (PureF f a) = f a
type instance Eval (f =<< a) = Eval (f (Eval a))
type instance Eval ((f <=< g) a) = Eval (f (Eval (g a)))

type instance Eval (TypEq a b) = TypEqImpl a b

type family TypEqImpl a b :: Bool where
  TypEqImpl a a = True 
  TypEqImpl a b = False

type instance Eval (Collapse a) = CollapseImpl a

type family CollapseImpl (a :: [Constraint]) :: Constraint where
  CollapseImpl '[] = ()
  CollapseImpl (x:xs) = (x, CollapseImpl xs)

type instance Eval (Map f (Just a)) = Just (Eval (f a))
type instance Eval (Map f Nothing) = Nothing

type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (x:xs)) = Eval (f x) : Eval (Map f xs)

type instance Eval (Map f (Right x)) = Right (Eval (f x))
type instance Eval (Map f (Left x)) = Left x

type instance Eval (Mappend '[] as) = as
type instance Eval (Mappend (x ': xs) as) = x : Eval (Mappend xs as)

type instance Eval (Mappend (a :: Constraint) (b :: Constraint)) = (a,b)

type instance Eval (Mempty (as :: [a])) = '[]

type instance Eval (Mempty (as :: Constraint)) = ()
