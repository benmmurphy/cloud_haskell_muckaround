module Remoting where

import Remote
import Control.Monad
import Control.Concurrent

initialProcess :: String -> ProcessM ()
initialProcess "MASTER" = do
  say "master!"
  peers <- getPeers
  say $ "peers: " ++ (show peers)
  let slaves = findPeerByRole peers "SLAVE"
  forM_ slaves $ \slave -> do
    Just slaveProc <- nameQuery slave "slave"
    send slaveProc "ping"

initialProcess "SLAVE" = do
  say "slave!"
  nameSet "slave"
  ping <- expect
  say $ "SLAVE GOT " ++ show (ping :: String)

initialProcess role = error $ "Unknown role " ++ role

main :: IO()
main = do 
  forkIO $ remoteInit (Just "slave") [] initialProcess
  threadDelay (1000 * 1000 * 1)
  remoteInit (Just "config") [] initialProcess
