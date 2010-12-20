{-# LANGUAGE ExistentialQuantification #-}

import Data.Binary
import Data.Typeable
import Data.ByteString.Lazy.Internal
import Control.Monad
import Data.Maybe

data Persona = Persona { 
				nombre::String, 
				apellido::String, 
				edad::Int
				} deriving (Show, Read, Typeable)

instance Binary Persona where 
	put p = do 	put (nombre p);
				put (apellido p);
				put (edad p)

	get = do 
			  n <- get 
			  a <- get 
			  e <- get 
			  return Persona {nombre=n,apellido=a,edad=e}
			  
p = Persona {nombre="P", apellido="T", edad=26}

class Impl a where 
   impl :: a -> [ByteString] -> IO ByteString

instance (Binary a) => Impl (IO a) where
    impl m [] = liftM encode m
   
instance (Binary a,Impl b) => Impl (a->b) where 
   impl f (x:xs) = impl (f (decode x)) xs
   
methods = [("add", addIO)]

call fname params = impl (fromJust $ lookup fname methods) params

addIO:: Int -> Int -> IO Int
addIO a b= return (a+b)

func:: Persona -> IO String
func p = return (nombre p)