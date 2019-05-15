module Network.Remote.Socket.Receiver
  (
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (forM_)
import qualified Network.Remote.Protocol.SimpleStream as S
import qualified Network.Remote.Protocol.SimpleStream.ByteString as S
import Network.Remote.Resource.Address
import Network.Remote.Resource.Group
import Network.Remote.Socket.MulticastSocket
import qualified Network.Socket.ByteString as B
import Network.Remote
import Network.Remote.Protocol
import qualified System.IO.Streams as Streams



data ReceiverConfig =
  ReceiverConfig
    { name :: Maybe String
    , size :: Int
    , addresses :: Addresses
    , socketManager :: MulticastSocketManager
    }


defaultReceiverConfig name Nothing = ReceiverConfig name 65536
defaultReceiverConfig name (Just size) = ReceiverConfig Nothing 65536


runReceiver::ReceiverConfig->[MulticastListener]->IO (Maybe RemotePacket)
runReceiver (ReceiverConfig m size addresses manager) listeners=
          do
            defaultIn<-inputStream <$> defaultMulticastSocket manager
            (bs,addr)<- Streams.read defaultIn
            i<-S.fromByteString bs
            sender<-S.readEnd i
            if m==Just sender then return Nothing
            else do
                  cmd<-S.read i
                  insertSockAddr sender addr



