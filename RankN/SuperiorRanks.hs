{-#LANGUAGE RankNTypes #-}

applyToFive :: Integral b => (forall a. Integral a => a -> a) -> b
applyToFive f = f 5

doThings :: (forall a. (Show a) => a -> String) -> String
doThings f = f 5 ++ f True

main = do
    putStrLn $ doThings (\x -> show x ++ "\n")


-- Poliformismo de Rank 0
showInt :: Int -> String
showInt = undefined

-- Poliformismo de Rank 1
show' :: a -> String
show' = undefined
