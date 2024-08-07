
data List a = Cons a (List a) | Nil deriving (Eq, Show)

instance Semigroup (List a) where
    Cons a as <> b =  a `Cons` (as <> b)
    Nil       <> b = b

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a r) = f a `Cons` (f <$> r)

instance Applicative List where
    pure a = Cons a Nil

    Cons f fs <*> Cons a as = f a `Cons` (fs <*> as)

instance Monad List where
    
    Nil >>= _ = Nil
    Cons a as >>= f = f a <> (as >>= f)

test :: List (Integer, Char)
test = do
    a <- Cons 1 $ Cons 3 $ Nil
    b <- Cons 'a' $ Cons 'b' $ Nil
    return (a,b)