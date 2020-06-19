{-# LANGUAGE OverloadedStrings #-}

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
  -- Open all sockets manually (errors might be aroused due to some unexpected network interfaces)
  --  withManager manager openAllSockets
  (MulticastConduit i o) <- withManager manager defaultMulticastConduit
  forkIO $ runConduit $ i .| mapC (\(Message a _) -> a) .| stdoutC
  -- This will send 1000 times "Hello" into UDP network
  runConduit $ yieldMany [1 .. 1000] .| mapMC (\x -> threadDelay 100 >> (return . B.pack . (++ "\n") . show $ x)) .| o
  print "The last number should be 1000."

printUTF8 :: (Show a) => a -> IO ()
printUTF8 = B.putStrLn . fromString . show

-- runConduit $ yieldMany [1..100] .| mapC (\int -> (CommonCmd, "Hi there: " <> (B.pack . show $ int))) .| broadcast "Alice" .| o

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