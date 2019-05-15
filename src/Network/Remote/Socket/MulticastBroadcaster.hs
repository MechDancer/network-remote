module Network.Remote.Socket.MulticastBroadcaster
  (
  ) where

import Control.Monad.Reader
import Data.ByteString
import Network.Remote.Protocol
import Network.Remote.Socket.MulticastSocket

data MulticastBroadcaster = MulticastBroadcaster
  { name :: String
  , size :: Int
  , socketsManager :: MulticastSocketManager
  }

broadcast :: (Command a) => a -> ByteString -> ReaderT MulticastBroadcaster IO ()
broadcast = undefined