module Network.Remote.Resource.GroupMonitor where

import Control.Monad (unless)
import qualified Data.ByteString as B
import Data.Functor (($>))
import Data.List (null)
import Network.Remote.Protocol
import qualified Network.Remote.Resource.Group as Group
import Network.Remote.Resource.Group (Group)
import qualified Network.Remote.Socket.Broadcaster as Broadcaster
import Network.Remote.Socket.Broadcaster (BroadcasterConfig)

-- TODO Unused, Evil star
yell :: BroadcasterConfig -> IO ()
yell config = Broadcaster.broadcast config YELL_ASK B.empty

detect :: Group -> Name -> Maybe BroadcasterConfig -> IO ()
detect group node m = do
  unless (null node) $ Group.withGroup group (Group.detect node) $> ()
  case m of
    (Just reply) -> Broadcaster.broadcast reply YELL_ACK B.empty
    Nothing -> return ()