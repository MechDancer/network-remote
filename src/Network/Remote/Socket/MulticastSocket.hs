{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Remote.Socket.MulticastSocket
  ( MulticastSocket (..),
    multicastSocketToConduit,
    MulticastSocketManager,
    MulticastConduit (..),
    newManager,
    withManager,
    openedSockets,
    defaultMulticastConduit,
    openSocket,
    openAllSockets,
    shutdownManager,
  )
where

import Conduit
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), runReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Internal as S
import Data.Foldable (forM_)
import Data.Conduit.Network.UDP
import qualified Data.HashTable.IO as H
import qualified Data.Map as M
import GHC.Generics (Generic)
import Network.Info
import Network.Multicast
import Network.Remote.Resource.Networks (scanNetwork)
import Network.Socket

type HashTable k v = H.BasicHashTable k v

-------------------------------------------------------------------
data MulticastSocket = MulticastSocket
  { -- | Receiver socket
    receiver :: !Socket,
    -- | Sender socket
    sender :: !Socket,
    -- | Multicast addr
    address :: !SockAddr
  }

data MulticastConduit m = MulticastConduit
  { input :: forall i. ConduitT i Message m (),
    output :: forall o. ConduitT ByteString o m ()
  }

multicastSocketToConduit :: MonadIO m => MulticastSocket -> MulticastConduit m
multicastSocketToConduit MulticastSocket {..} = MulticastConduit i o
  where
    i = sourceSocket receiver 4096
    o = mapC (`Message` address) .| sinkAllToSocket sender

data MulticastSocketManager = Mgr
  { groupAddr :: !(HostName, PortNumber),
    core :: !(HashTable NetworkInterface MulticastSocket)
  }

-- | Create a multicast socket manager with group INET addr.
newManager :: HostName -> PortNumber -> IO MulticastSocketManager
newManager host port = H.new >>= \core -> return $ Mgr (host, port) core

-- | A simple way to access 'ReaderT MulticastSocketManager'.
withManager :: (MonadIO m) => MulticastSocketManager -> ReaderT MulticastSocketManager m a -> m a
withManager = flip runReaderT

-- | This DOES NOT work!
{-# DEPRECATED #-}
shutdownManager :: (MonadIO m) => ReaderT MulticastSocketManager m ()
shutdownManager = openedSockets >>= liftIO . mapM_ (go . snd)
  where
    go MulticastSocket {..} = close' sender >> close' receiver

-- | Get all opened sockets.
openedSockets ::
  (MonadIO m) =>
  ReaderT MulticastSocketManager m [(NetworkInterface, MulticastSocket)]
openedSockets = ReaderT $ liftIO . H.toList . core

mkMulticastConduit ::
  (MonadIO m) =>
  HostName ->
  PortNumber ->
  Maybe NetworkInterface ->
  m (MulticastConduit m)
mkMulticastConduit host port m = liftIO $ do
  (s, addr) <-  multicastSender host port
  r <-  multicastReceiver host port
  forM_ m (setInterface s . show . ipv4)
  return $ multicastSocketToConduit $ MulticastSocket r s addr

-- | Get the default multicast socket retrieving all packets (Manager will /not/ hold this socket).
defaultMulticastConduit ::
  (MonadIO m) => ReaderT MulticastSocketManager m (MulticastConduit m)
defaultMulticastConduit =
  ReaderT $ \(Mgr (host, port) _) -> mkMulticastConduit host port Nothing

-- | Open and save a multicast socket with a specific network interface.
openSocket ::
  (MonadIO m) =>
  NetworkInterface ->
  ReaderT MulticastSocketManager m MulticastSocket
openSocket net = ReaderT $ \(Mgr (host, port) var) -> liftIO $ do
  result <- H.lookup var net
  case result of
    Just x -> return x
    Nothing -> go
      where
        go = do
          (s, addr) <- multicastSender host port
          r <- liftIO $ multicastReceiver host port
          setInterface s . show . ipv4 $ net
          let result = MulticastSocket r s addr
          H.insert var net result
          return result

-- | Open all network interfaces.
openAllSockets :: (MonadIO m) => ReaderT MulticastSocketManager m [MulticastSocket]
openAllSockets = liftIO scanNetwork >>= mapM openSocket
