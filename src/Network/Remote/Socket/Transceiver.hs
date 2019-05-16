module Network.Remote.Socket.Transceiver
  ( BroadcasterConfig(..)
  , defaultBroadcasterConfig
  , broadcast
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (forM_)
import Data.Maybe (isNothing)
import Network.Remote.Protocol
import qualified Network.Remote.Protocol.SimpleStream as S
import qualified Network.Remote.Protocol.SimpleStream.ByteString as S
import Network.Remote.Socket.MulticastSocket
import qualified Network.Socket.ByteString as B
import qualified System.IO.Streams as Streams

import Network.Remote
import Network.Remote.Resource.Address
import Network.Remote.Resource.Group

data BroadcasterConfig = BroadcasterConfig
  { name :: Maybe String
  , size :: Int
  , socketManager :: MulticastSocketManager
  }

defaultBroadcasterConfig name Nothing = BroadcasterConfig name 0x4000
defaultBroadcasterConfig name (Just size) = BroadcasterConfig Nothing size

broadcast :: (Command a) => BroadcasterConfig -> a -> ByteString -> IO ()
broadcast (BroadcasterConfig m size manager) cmd payload =
  if isNothing m && (cmd =.= YELL_ACK || cmd =.= ADDRESS_ACK)
    then print "No name"
    else do
      stream <- S.empty size
      case m of
        (Just name) -> S.writeEnd stream name
        Nothing -> return ()
      S.writeList stream $ B.unpack payload
      list <- S.toList stream
      socks <- openedSockets manager
      forM_ socks (Streams.write (Just $ B.pack list) . outputStream . snd)

-------------------------------------------------------------------
data ReceiverConfig = ReceiverConfig
  { _name :: Maybe String
  , _size :: Int
  , _addresses :: Addresses
  , _socketManager :: MulticastSocketManager
  }

defaultReceiverConfig name Nothing = ReceiverConfig name 65536
defaultReceiverConfig name (Just size) = ReceiverConfig Nothing 65536

runReceiver :: ReceiverConfig -> [MulticastListener] -> IO (Maybe RemotePacket)
runReceiver (ReceiverConfig m size addresses manager) listeners = do
  defaultIn <- inputStream <$> defaultMulticastSocket manager
  mPacket <- Streams.read defaultIn
  if isNothing mPacket then
    return Nothing
    else do
      let Just (bs, addr) = m
      i <- S.fromByteString bs
      sender <- S.readEnd i
      if m == Just sender
        then return Nothing
        else do
          cmd <- S.read i
          rest <- S.lookRest i
          insertSockAddr addresses sender addr
              -- TODO open the socket of network interface with matched address
          let filtered = filter (\l -> null (interest l) || cmd `elem` interest l) listeners
              packet = RemotePacket sender cmd (B.pack rest)
              in mapM_ (`process` packet) filtered
          return $ Just (RemotePacket sender cmd (B.pack rest))
