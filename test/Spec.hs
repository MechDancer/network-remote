{-# LANGUAGE OverloadedStrings #-}

import Codec.Binary.UTF8.String (encodeString)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as B
import Network.Info (NetworkInterface, ipv4, name)
import Network.Remote.Protocol (CommonCmd (..), RemotePacket (..), multicastListener)
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
  -- Open all sockets manually | Errors might be aroused sometimes
  --  withManager manager openAllSockets
  case head role of
    "r" -> runReceiverForever addresses networks manager
    "b" -> runBroadcasterForever manager

runReceiverForever :: Addresses -> [NetworkInterface] -> MulticastSocketManager -> IO ()
runReceiverForever addresses networks manager = let receiveConfig = defaultReceiverConfig (Just "HaskellR") Nothing addresses networks manager in forever $ runReceiver receiveConfig [multicastListener [CommonCmd] print]

runBroadcasterForever :: MulticastSocketManager -> IO ()
runBroadcasterForever manager = let broadcastConfig = defaultBroadcasterConfig (Just "HaskellB") Nothing manager in forever $ broadcast broadcastConfig CommonCmd ("Hello.") >> threadDelay 500