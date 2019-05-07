module Network.Remote.MulticastSocket where

import Network.Multicast
import Network.Socket

import Control.Concurrent.MVar
import Data.Foldable (forM_)
import qualified Data.Map as M

data MulticastSocket = MulticastSocket
  { receiver :: !Socket
  , sender :: !Socket
  , address :: !SockAddr
  }

data MulticastSocketManager = Mgr
  { groupAddr :: (HostName, PortNumber)
  , coreM :: IO (MVar (M.Map HostName MulticastSocket))
  }

-- | Create a multicast socket manager with group INET addr
newManager :: HostName -> PortNumber -> IO MulticastSocketManager
newManager host port = return $ Mgr (host, port) (newMVar M.empty)

-- | Create a multicast socket containing sender an receiver
multicastOn ::
     HostName -- Group host
  -> PortNumber -- Group port
  -> Maybe HostName -- Network interface
  -> IO MulticastSocket
multicastOn host port m = do
  (s, addr) <- multicastSender host port
  r <- multicastReceiver host port
  forM_ m (setInterface s)
  forM_ m (setInterface r)
  return $ MulticastSocket s r addr

defaultMulticastSocket :: MulticastSocketManager -> IO MulticastSocket
defaultMulticastSocket (Mgr (host, port) _) = multicastOn host port Nothing

getWithInterface :: MulticastSocketManager -> HostName -> IO MulticastSocket
getWithInterface (Mgr (host, port) var) net = do
  result <- multicastOn host port (Just net)
  mCore <- var
  modifyMVar_ mCore $ \core -> return $ M.insert net result core
  return result