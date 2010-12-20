-- A simple RPC example which uses Show and Read for serialization.
-- This should at least have some error checking to be useful.

import Control.Monad
import Data.Maybe
import Network
import System.IO

type Call = (MethodName,[Argument])
type MethodName = String
type Argument = String
type Reply = String

-- * Client side library

remote :: Remote a => HostName -> PortID -> MethodName -> a
remote host port m = remote_ (\xs -> call host port (m,xs))
    where call host port c = do h <- connectTo host port
                                hPutStrLn h (show c)
                                hFlush h
                                l <- hGetLine h
                                hClose h
                                return l

class Remote a where
    remote_ :: ([Argument] -> IO Reply) -> a
    
instance Read a => Remote (IO a) where
    remote_ f = liftM read (f [])

instance (Show a, Remote b) => Remote (a -> b) where
    remote_ f x = remote_ (\xs -> f (show x:xs))



-- * Server side library

type Method = (MethodName, [Argument] -> IO Reply)

class Impl a where
    impl :: a -> [Argument] -> IO Reply

instance Show a => Impl (IO a) where
    impl m [] = liftM show m

instance (Read a,Impl b) => Impl (a -> b) where
    impl f (x:xs) = impl (f (read x)) xs

method :: Impl a => MethodName -> a -> Method
method n f = (n, impl f)

handleCall :: [Method] -> Call -> IO Reply
handleCall ms (n,xs) = ($ xs) $ fromJust $ lookup n ms

server :: PortID -> [Method] -> IO ()
server port ms = listenOn port >>= loop
   where loop s = do (h,_,_) <- accept s
                     oneCall h
                     loop s
         oneCall h = do l <- hGetLine h
                        r <- handleCall ms (read l)
                        hPutStrLn h r
                        hClose h


-- * Client side example

add :: Int -> Int -> IO Int
add = remote "localhost" (PortNumber 12345) "add"


-- * Server side example

addImpl :: Int -> Int -> IO Int
addImpl x y = return (x+y)

testServer :: IO ()
testServer = server (PortNumber 12345) [method "add" addImpl]

{-

Example session:

$ ghc -e testServer simplerpc.hs

(in another shell:)

$ ghc -e "add 40 2" simplerpc.hs 
42

-}