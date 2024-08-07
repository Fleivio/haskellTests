
data State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
    fmap f s1 = 
        State $ \s -> 
            let (s2, a) = runState s1 s
            in (s2, f a)

instance Applicative (State s) where
    pure a = State $ (,a)

    sf <*> sx = 
        State $ \so ->
            let (so1, f) = runState sf so
                (so2, x) = runState sx so1
            in (so1, f x)

instance Monad (State s) where
    sx >>= g = 
        State $ \so -> 
            let (so1, x) = runState sx so
                sy = g x
            in runState sy so1

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())

put :: s -> State s ()
put = modify . const

-------------------------------------------

data QueueCalc a = QC {queue :: [a]} deriving Show

pop' :: QueueCalc a -> (QueueCalc a, a)
pop' (QC (x:xs)) = (QC xs, x)

push' :: QueueCalc a -> a -> QueueCalc a
push' (QC xs) x = QC (x:xs)

over' :: QueueCalc a -> (a -> a -> a) -> QueueCalc a
over' (QC (x:y:xs)) f = QC (f x y : xs)

test' :: QueueCalc Int
test' = let a = QC [1,2,3,4]
            (a2, x) = pop' a
            a3 = push' a2 7
            a4 = over' a3 (+)
            a5 = over' a4 (*)
        in a5

--------------------------------------------------
-- data State s a = State {runState :: s -> (s, a)}

pop :: State (QueueCalc a) a
pop = State $ \(QC (x:xs)) -> (QC xs, x)

push :: a -> State (QueueCalc a) a
push a = State $ \(QC xs) -> (QC (a:xs), a)

over :: (a -> a -> a) -> State (QueueCalc a) a
over f = State $ \(QC (x : y : xs)) -> (QC (f x y : xs), f x y)

onto :: (a -> a) -> State (QueueCalc a) a
onto f = State $ \(QC (x : xs)) -> (QC (f x: xs), f x)

setToF :: ([a] -> a) -> State (QueueCalc a) ()
setToF f = State $ \(QC a) -> (QC [f a], ())

setToSum :: State (QueueCalc Int) ()
setToSum = setToF sum

setToZero :: State (QueueCalc Int) ()
setToZero = setToF $ const 0

test :: State (QueueCalc Int) Int
test = do
    onto (*2)
    over (+)
    push 1
    over div



test2 :: State (QueueCalc Int) [Int]
test2 = sequence
    [onto (*2),
    pop,
    over (+),
    push 1,
    over div]

main :: IO ()
main = print $ runState test (QC [1,2,3,4])