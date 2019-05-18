module Network.Remote.Socket.MulticastSocket
  ( MulticastSocket(..)
  , SocketStream(..)
  , multicastSocketToStream
  , MulticastSocketManager
  , newManager
  , withManager
  , openedSockets
  , defaultMulticastSocket
  , getWithInterface
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Internal as S
import Data.Foldable (forM_)
import qualified Data.HashTable.IO as H
import qualified Data.Map as M
import Network.Info
import Network.Multicast
import Network.Remote.Resource.Networks () -- ^ import the instance of `Eq`
import Network.Socket
import qualified Network.Socket.ByteString as B
import qualified System.IO.Streams as Streams
import System.IO.Streams (InputStream, OutputStream)

type HashTable k v = H.BasicHashTable k v

-------------------------------------------------------------------
data MulticastSocket = MulticastSocket
  { receiver :: !Socket
  , sender :: !Socket
  , address :: !SockAddr
  }

data SocketStream = SocketStream
  { inputStream :: InputStream (ByteString, SockAddr)
  , outputStream :: OutputStream ByteString
  }

data MulticastSocketManager = Mgr
  { _groupAddr :: (HostName, PortNumber)
  , _core :: HashTable NetworkInterface SocketStream
  }

-------------------------------------------------------------------
multicastSocketToStream :: MulticastSocket -> IO SocketStream
multicastSocketToStream (MulticastSocket sender receiver addr) = do
  input <- socketToStreamsInternalI receiver
  output <- socketToStreamsInternalO sender addr
  return $ SocketStream input output

socketToStreamsInternalI socket = Streams.makeInputStream input
  where
    input = do
      (s, addr) <- B.recvFrom socket 4096
      return $!
        if S.null s
          then Nothing
          else Just (s, addr)

socketToStreamsInternalO socket addr = Streams.makeOutputStream output
  where
    output Nothing = return ()
    output (Just s) =
      if S.null s
        then return ()
        else B.sendAllTo socket s addr

-------------------------------------------------------------------
-- | Create a multicast socket manager with group INET addr
newManager :: HostName -> PortNumber -> IO MulticastSocketManager
newManager host port = H.new >>= \core -> return $ Mgr (host, port) core

-- | Simple way to access `ReaderT`
withManager :: MulticastSocketManager -> ReaderT MulticastSocketManager m a -> m a
withManager = flip runReaderT

-- | Get all opened sockets
openedSockets :: (MonadIO m) => ReaderT MulticastSocketManager m [(NetworkInterface, SocketStream)]
openedSockets = ReaderT $ liftIO . H.toList . _core

-- | Create a multicast socket containing sender an receiver
multicastOn ::
     HostName -- ^ Group host
  -> PortNumber -- ^ Group port
  -> Maybe NetworkInterface -- ^ Network interface (Optional)
  -> IO SocketStream
multicastOn host port m = do
  (s, addr) <- multicastSender host port
  r <- multicastReceiver host port
  forM_ m (setInterface s . show . ipv4)
  multicastSocketToStream $ MulticastSocket s r addr

defaultMulticastSocket :: (MonadIO m) => ReaderT MulticastSocketManager m SocketStream
defaultMulticastSocket = ReaderT $ \(Mgr (host, port) _) -> liftIO $ multicastOn host port Nothing

getWithInterface :: (MonadIO m) => NetworkInterface -> ReaderT MulticastSocketManager m SocketStream
getWithInterface net =
  ReaderT $ \(Mgr (host, port) var) ->
    liftIO $ do
      result <- multicastOn host port (Just net)
      H.insert var net result
      return result