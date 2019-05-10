module Network.Remote.Resource.MulticastSocket
  ( MulticastSocket(..)
  , MulticastSocketManager
  , newManager
  , defaultMulticastSocket
  , getWithInterface
  ) where

import Network.Multicast
import Network.Socket

import Data.Foldable (forM_)
import Data.IORef
import qualified Data.Map as M
import Network.Info
import Network.Remote.Resource.Networks

data MulticastSocket = MulticastSocket
  { receiver :: !Socket
  , sender :: !Socket
  , address :: !SockAddr
  }

data MulticastSocketManager = Mgr
  { _groupAddr :: (HostName, PortNumber)
  , _core :: IORef (M.Map NetworkInterface MulticastSocket)
  }

-- | Create a multicast socket manager with group INET addr
newManager :: HostName -> PortNumber -> IO MulticastSocketManager
newManager host port = newIORef M.empty >>= \core -> return $ Mgr (host, port) core

-- | Create a multicast socket containing sender an receiver
multicastOn ::
     HostName -- ^ Group host
  -> PortNumber -- ^ Group port
  -> Maybe NetworkInterface -- ^ Network interface (Optional)
  -> IO MulticastSocket
multicastOn host port m = do
  (s, addr) <- multicastSender host port
  r <- multicastReceiver host port
  forM_ m (setInterface s . show . ipv4)
  return $ MulticastSocket s r addr

defaultMulticastSocket :: MulticastSocketManager -> IO MulticastSocket
defaultMulticastSocket (Mgr (host, port) _) = multicastOn host port Nothing

getWithInterface :: MulticastSocketManager -> NetworkInterface -> IO MulticastSocket
getWithInterface (Mgr (host, port) var) net = do
  result <- multicastOn host port (Just net)
  modifyIORef var $ M.insert net result
  return result