
---[State]---------------------------------------------------------------

data State a b =
    State {runState :: a -> (a, b)}

evalState s a = fst $ runState s a

instance Functor (State a) where
    fmap f g = State $ \x ->
        let (a, b) = runState g x
        in  (a, f b)

instance Applicative (State a) where
    pure a = State (, a)

    f <*> g = State $ \x ->
        let (fa, fb) = runState f x
            (ga, gb) = runState g fa
        in (ga, fb gb)

instance Monad (State a) where
    return = pure

    g >>= f = State $ \x ->
        let (ga, gb) = runState g x
            st2      = f gb
        in runState st2 ga

modify :: (a -> a) -> State a a
modify f = State $ \x -> (f x, f x)

get :: State a a
get = modify id

test :: State Int Int
test = 
    modify (+1) >>
    modify (+1) >>
    get
    

---------------------------------------------------------

data Stack a = Stack [a]
    deriving (Eq, Show)

pop :: State (Stack a) a
pop = State $ \(Stack x) -> (Stack $ tail x, head x)

push :: a -> State (Stack a) a
push x = State $ \(Stack xs) -> (Stack $ x:xs, x)

app :: (a -> a -> a) -> State (Stack a) a
app f = State $ \(Stack (x:y:xs)) -> let f1 = f x y
                                     in (Stack $ f1:xs, f1)

app' :: (a -> a) -> State (Stack a) a
app' f = State $ \(Stack (x:xs)) -> let f1 = f x
                                     in (Stack $ f1:xs, f1)


opp :: State (Stack Int) Int
opp = do
    x <- app (+)
    app' (+x)


---------------------------------

{-
cada operação altera um estado global
-}
data W

data Op a = Op {rop :: W -> (a,W) }

instance Functor Op where
    fmap f g= Op $ \w ->
        let (a,w1) = rop g w
        in (f a, w1)

instance Applicative Op where
    pure a = Op (a,)

    g <*> h = Op $ \w ->
        let (ha, hw) = rop h w
            (ga, gw) = rop g hw
        in (ga ha, gw)

instance Monad Op where
    return = pure

    g >>= f = Op $ \w ->
        let (ga, gw) = rop g w
            h = f ga
        in rop h gw
