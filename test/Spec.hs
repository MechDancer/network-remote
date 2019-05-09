{-# LANGUAGE OverloadedStrings #-}

import Network.Remote.Protocol.ZigZag

import Control.Monad
import Network.Remote
<<<<<<< HEAD
import Data.Time.Clock.System
import Network.Multicast
import Network.Socket
import Control.Monad
import qualified Network.Socket.ByteString as B

main :: IO ()
main = do
--  manager <- newManager "233.33.33.33" 23333
  sock <- multicastReceiver "233.33.33.33" 23333
  forever $ do
    (msg, addr) <- B.recvFrom sock 1024
    print (msg, addr)
=======
import Network.Remote.Resource.Networks

main :: IO ()
main = do
  scan>>=print
  currentTimeSeconds >>= print
  print $ decodeN . encodeN $ [-1, 2, 3]
  return ()
>>>>>>> 72ccd35c5ba9d9d8abff5a75586f174e66aaf4f0
