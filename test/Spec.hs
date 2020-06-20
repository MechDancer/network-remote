{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Conduit
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.UTF8 (fromString)
import Data.Conduit.Network.UDP
import Network.Info (NetworkInterface, ipv4, name)
import Network.Multicast
import Network.Remote
import Network.Remote.Protocol.Conduit.ByteString
import Network.Remote.Resource.Address
import Network.Remote.Resource.Networks (scanNetwork)
import Network.Remote.Socket.MulticastSocket
import System.Environment

main :: IO ()
main = do
  manager <- newManager "233.33.33.33" 23333
  addresses <- newAddresses
  networks <- scanNetwork
  print "Scanning network..."
  print networks
  let terminalName = "Alice"
      receiverConfig = ReceiverConfig "Bob" addresses networks manager

  -- Open all sockets manually (errors might be aroused due to some unexpected network interfaces)
  withManager manager openAllSockets

  -- Receiver "Bob"
  forkIO $ runConduit $
    receivePacket receiverConfig
      .| mapC (\RemotePacket {..} -> "[Bob] Receive packet from " ++ sender ++ ", with command " ++ show command ++ " , and payload" ++ show payload)
      .| printC
  -- Sender "Alice"
  runConduit $
    yieldMany [1 .. 1000]
      .| mapMC (\x -> threadDelay 100 >> (return . B.pack . (++ "\n") . show $ x))
      .| mapC (CommonCmd,)
      .| mapMC (\x@(cmd, pack) -> print ("[Alice] Send packet with command " ++ show cmd ++ " and payload " ++ show pack) >> return x)
      .| broadcast "Alice" manager
  -- Wait last packet
  threadDelay 100

printUTF8 :: (Show a) => a -> IO ()
printUTF8 = B.putStrLn . fromString . show

-- From 'network-multicast'
{- nativeTest = do
  MulticastSocket ii oo addr <- openSocket'
  forkIO . forever $ recvFrom ii 23333 >>= \(a, _) -> print a
  forkIO . forever $ sendAllTo oo "Hello" addr
  return ()

openSocket' = do
  (s, addr) <- multicastSender "233.33.33.33" 23333
  r <- multicastReceiver "233.33.33.33" 23333
  scanNetwork >>= setInterface s . show . ipv4 . head
  let result = MulticastSocket s r addr
  return result -}