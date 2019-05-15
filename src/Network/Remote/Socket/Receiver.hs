module Network.Remote.Socket.Receiver
  (
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (forM_)
import Network.Remote.Protocol
import qualified Network.Remote.Protocol.SimpleStream as S
import Network.Remote.Protocol.SimpleStream.ByteString
import Network.Remote.Resource.Address
import Network.Remote.Resource.Group
import Network.Remote.Socket.MulticastSocket
import qualified Network.Socket.ByteString as B

data ReceiverConfig =
  ReceiverConfig
    { name :: Maybe String
    , size :: Int
    , group :: Group
    , addresses :: Addresses
    , socketsManager :: MulticastSocketManager
    }

defaultReceiverConfig name Nothing = ReceiverConfig name 65536
defaultReceiverConfig name (Just size) = ReceiverConfig Nothing 65536


--runReceiver::ReceiverConfig->
--runReceiver ReceiverConfig(m size group addresses manager) =