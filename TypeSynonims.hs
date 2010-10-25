{-TypeSynonymInstances -}
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.STM
import Control.Concurrent.STM.TVar

test :: StateT String IO String
test = do 
        x <- liftIO $ getLine
	y <- liftIO $ prbSTM x
        return (concat (x:y:[]))

prbSTM :: String -> IO String
prbSTM x = atomically $ (return "aaa")
	   
instance Num String where
	a + b = concat (a:b:[])
	a * b = a
	a - b = a
	negate a = a
	abs a = a
	signum a = a
	fromInteger n = show n