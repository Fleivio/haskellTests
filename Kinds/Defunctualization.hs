{-#LANGUAGE FunctionalDependencies, UndecidableInstances #-}

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

data LTM a = LTM [a] deriving (Eq, Show)
data Fst a b = Fst (a,b) deriving (Eq, Show)
data MapList b a = MapList (a -> b) [a] 

class Eval t l | t -> l where
  eval :: t -> l

instance Eval (LTM a) (Maybe a) where
  eval (LTM []) = Nothing
  eval (LTM (x:xs)) = Just x

instance Eval (Fst a b) a where
  eval (Fst (a,b)) = a

instance Eval a c => Eval (MapList a b) [c] where
  eval (MapList f []) = []
  eval (MapList f (x:xs)) = eval (f x) : eval (MapList f xs)