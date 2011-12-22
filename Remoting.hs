module Remoting where

import Remote
import Control.Monad

initialProcess :: String -> ProcessM ()
initialProcess "MASTER" = do
  forM_ [1..10] $ \i -> do
    (sp, rp) <- newChannel
    pid <- spawnLocal (sayHello rp)
    sendChannel sp  (i :: Int)

initialProcess role = error $ "Unknown role " ++ role


sayHello :: ReceivePort Int -> ProcessM ()
sayHello rp =  do 
  pid <- getSelfPid
  n <- receiveChannel rp
  say $ "Hello World " ++ show n

main :: IO()
main = remoteInit (Just "config") [] initialProcess
