module Network.Remote.Socket.Receiver
  ( ReceiverConfig(..)
  , defaultReceiverConfig
  , runReceiver
  ) where

import           Data.ByteString                                 (ByteString)
import qualified Data.ByteString                                 as B
import           Data.Foldable                                   (forM_)
import           Data.Maybe                                      (fromJust,
                                                                  isNothing)
import           Network.Remote
import           Network.Remote.Protocol
import qualified Network.Remote.Protocol.SimpleStream            as S
import qualified Network.Remote.Protocol.SimpleStream.ByteString as S
import           Network.Remote.Resource.Address
import           Network.Remote.Socket.MulticastSocket
import qualified Network.Socket.ByteString                       as B
import qualified System.IO.Streams                               as Streams

data ReceiverConfig =
  ReceiverConfig
    { _name          :: Maybe String
    , _size          :: Int
    , _addresses     :: Addresses
    , _socketManager :: MulticastSocketManager
    }

defaultReceiverConfig name Nothing = ReceiverConfig name 65536

runReceiver :: ReceiverConfig -> [MulticastListener] -> IO (Maybe RemotePacket)
runReceiver (ReceiverConfig m size addresses manager) listeners = do
  defaultIn <- inputStream <$> withManager manager defaultMulticastSocket
  mPacket <- Streams.read defaultIn
  if isNothing mPacket
    then return Nothing
    else do
      let Just (bs, addr) = mPacket
      i <- S.fromByteString bs
      sender <- S.readEnd i
      -- Packet sent by myself
      if m == Just sender
        then return Nothing
        else do
          cmd <- S.read i
          rest <- S.lookRest i
          withAddresses addresses $ insertSockAddr sender addr
          -- TODO open the corresponding socket of network interface with matched address
          let filtered = filter (\l -> null (interest l) || cmd `elem` interest l) listeners
              packet = RemotePacket sender cmd (B.pack rest)
          mapM_ (`process` packet) filtered
          return . Just $ packet
