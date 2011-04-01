import  Language.Haskell.TH
import  Data.Typeable
import  Control.Monad.IO.Class
import  Language.Haskell.Extract

import Maybe

data PM a = PM (Int -> String -> IO (a, Int, String))

instance Monad PM where
   return x = PM(\v -> \s -> return (x,v,s))
   (>>=) (PM m) f' = PM (\r -> \s -> do (x,v,s') <- m r s; sacar (f' x) v s')

instance MonadIO PM where
   liftIO f = PM(\v -> \s -> do x <- f; return (x,v,s++"liftIO"))

sacar (PM m) v s = m v s

add :: PM ()
add = PM (\x -> \s -> return ((),x+1,s++"add"))

get :: PM Int
get = PM (\x -> \s -> return (x,x,s++"get"))

runPM m = do (x,v,s) <- (sacar m) 0 ""; print s; return x

prueba x y = x + y

main = do 
          putStrLn $(stringE . pprint =<< (reify $ mkName "runPM"))
          putStrLn $(stringE . pprint =<< (reify $ mkName "prueba"))
          putStrLn $(stringE . pprint =<< (reify $ mkName "get"))
          putStrLn $(stringE . pprint =<< (reify $ mkName "PM"))
	  putStrLn $ show $(functionExtractorMap "" [| \n t -> n |])


