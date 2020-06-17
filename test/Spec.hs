{-# LANGUAGE OverloadedStrings #-}

import Codec.Binary.UTF8.String (encodeString)
import Conduit
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as B
import Network.Info (NetworkInterface, ipv4, name)
import Network.Remote.Protocol (CommonCmd (..), RemotePacket (..))
import Network.Remote.Resource.Address
import Network.Remote.Resource.Networks (scanNetwork)
import Network.Remote.Socket.Broadcaster (broadcast, defaultBroadcasterConfig)
import Network.Remote.Socket.MulticastSocket
import Network.Remote.Socket.Receiver (defaultReceiverConfig, runReceiver)
import System.Environment

main :: IO ()
main = do
  role <- getArgs
  manager <- newManager "233.33.33.33" 23333
  addresses <- newAddresses
  networks <- scanNetwork
  print networks
  -- Open all sockets manually (errors might be aroused due to some unexpected network interfaces)
  --  withManager manager openAllSockets
  (MulticastConduit i o) <- withManager manager defaultMulticastConduit
  -- This will send 10 times "Hello" into UDP network
  runConduit $ replicateC 10 "Hello" .| o
  return ()
