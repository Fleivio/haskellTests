
data RWS r w s a = RWS {runRWS :: r -> s -> (s,w,a)}

instance Functor (RWS r w s) where
    fmap f (RWS g) = 
        RWS $ \r s -> 
            let (s2, w, a) = g r s
            in (s2, w, f a)

instance Monoid w => Applicative (RWS r w s) where
    pure a = RWS $ \r s -> (s, mempty, a)

    (RWS g) <*> (RWS x) =
        RWS $ \r s ->
            let (s1, w1, f1) = g r s
                (s2, w2, x2) = x r s1
            in (s2, w1 <> w2, f1 x2)

instance Monoid w => Monad (RWS r w s) where
    return = pure

    (RWS f) >>= g = 
        RWS $ \r s ->
            let (s1, w1, x1) = f r s
                (RWS h) = g x1
            in h r s1

data Environtment = Env {
        temperature :: Double,
        pressure :: Double,
        umidity :: Double
    } deriving (Eq, Show)

tell :: w -> RWS r w s ()
tell w = RWS $ \r s -> (s, w, ())

asks :: Monoid w => (r -> a) -> RWS r w s a
asks f = RWS $ \r s -> (s, mempty, f r)

ask :: Monoid w => RWS r w s r
ask = asks id

gets :: Monoid w => (s -> a) -> RWS r w s a
gets f = RWS $ \r s -> (s, mempty, f s)

get :: Monoid w => RWS r w s s
get = gets id 

modify :: Monoid w => (s -> s) -> RWS r w s ()
modify f = RWS $ \r s -> (f s, mempty, ())

put :: Monoid w => s -> RWS r w s ()
put = modify . const
