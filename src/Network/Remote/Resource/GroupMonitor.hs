module Network.Remote.Resource.GroupMonitor where

import qualified Data.ByteString as B
import Network.Remote.Protocol
import qualified Network.Remote.Resource.Group as Group
import Network.Remote.Resource.Group (Group)
import qualified Network.Remote.Socket.Transceiver as Transceiver
import Network.Remote.Socket.Transceiver (BroadcasterConfig)
import Data.List
yell :: BroadcasterConfig -> IO ()
yell config = Transceiver.broadcast config YELL_ASK B.empty

--detect :: Group -> Name -> Maybe Bool -> IO ()
--detect group node m = if no