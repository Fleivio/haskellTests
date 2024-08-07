data Cont r a = Cont
  { runCont :: (a -> r) -> r
  }

evalCont :: Cont a a -> a
evalCont = runCont `flip` id

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f =
  Cont $ \cc ->
    let Cont g = f (Cont . const . cc)
     in g cc

label :: a -> Cont r (a -> Cont r b, a)
label a =
  callCC $ \k ->
    let go b = k (go, b)
     in return (go, a)

instance Functor (Cont r)
 where
  fmap f (Cont g) = Cont $ \cc -> g (cc . f)

instance Applicative (Cont r)
 where
  pure a  = Cont $ \f -> f a
  f <*> x = Cont $ \b -> runCont f (\ab -> runCont (ab <$> x) b)

instance Monad (Cont r)
 where
  return       = pure
  Cont g >>= f = Cont $ \cc -> g $ \a -> runCont (f a) cc

-----------------------------------
factCPS :: Int -> Cont r Int
factCPS 0 = pure 1
factCPS n = (n *) <$> factCPS (n - 1)

foo :: Cont r Int
foo = do
  x <- factCPS 3
  y <- factCPS 4
  return (x + y)


--------------------------------------------------
applyTo3 :: Cont r Int
applyTo3 = pure 3

test3 :: Cont r Int
test3 = do
  x <- applyTo3
  y <- applyTo3
  return (x + y)


--------------------------------------------------
whatsYourName :: String -> String
whatsYourName name =
  (`runCont` id) $ do
    a <-
      callCC $ \exit -> do
        validateName name exit
        return $ "Welcome, " ++ name
    return (a ++ "!")

validateName :: (Foldable t, Monad m) => t a -> (String -> m ()) -> m ()
validateName name exit = do
  if null name
    then exit "You forgot to tell me your name!"
    else return ()
