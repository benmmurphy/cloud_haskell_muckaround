module Remoting where

import Remote
import Control.Monad

initialProcess :: String -> ProcessM ()
initialProcess "MASTER" = do
  forM_ [1..10] $ \i -> do
    pid <- spawnLocal sayHello
    send pid (i :: Int)
    send pid (fromIntegral i :: Double)
initialProcess role = error $ "Unknown role " ++ role


sayHello :: ProcessM ()
sayHello = do 
  pid <- getSelfPid
  sequence_ . replicate 2 $ receiveWait [ 
    match $ \n  ->  
      say $ "Hello World from " ++ show pid ++ show (n :: Int),
    matchUnknown (say "Unknown message")]

main :: IO()
main = remoteInit (Just "config") [] initialProcess
