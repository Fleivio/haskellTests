
import Control.Monad.Cont
import Control.Monad.IO.Class
import Control.Monad

nome :: String -> String
nome name = 
    (`runCont` id) $ do
        a <- callCC $ \exit -> do
            validateName name exit
            exit $ "bien-venido " <> name
            return ""
        return $ a <> "!" 

validateName :: String -> (String -> Cont r ()) -> Cont r ()
validateName name exit = do
    if length name > 10
        then exit "nome muito grande"
        else return ()


whatsYourNameLabel :: IO ()
whatsYourNameLabel = evalContT $ do
  (beginning, attempts) <- label (0 :: Int)
  liftIO $ putStrLn $ "Attempt #" <> show attempts
  liftIO $ putStrLn $ "What's your name?"
  name <- liftIO getLine
  when (null name) $ beginning (attempts + 1)
  liftIO $ putStrLn $ "Welcome, " ++ name ++ "!"