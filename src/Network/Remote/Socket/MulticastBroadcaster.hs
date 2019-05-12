module Network.Remote.Socket.MulticastBroadcaster
    (
    ) where

import Network.Multicast
import Network.Socket
import Network.Remote.Socket.MulticastSocket
import Data.IORef

data MulticastBroadcaster = MulticastBroadcaster {
  name ::String,
  size :: Int,
  socketsManager :: IORef MulticastSocketManager
}

-- encode :: (Command a)=>String
--   ->a
--   ->[Word8]
--   ->[ByteString]
-- encode name cmd payload=if (length name)+1+(length payload)
