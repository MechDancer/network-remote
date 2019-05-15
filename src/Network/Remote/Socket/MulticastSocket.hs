module Network.Remote.Socket.MulticastSocket
  ( MulticastSocket(..)
  , SocketStream(..)
  , multicastSocketToStream
  , MulticastSocketManager
  , newManager
  , openedSockets
  , defaultMulticastSocket
  , getWithInterface
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Internal as S
import Data.Foldable (forM_)
import qualified Data.HashTable.IO as H
import qualified Data.Map as M
import Network.Info
import Network.Multicast
import Network.Socket
import qualified Network.Socket.ByteString as B
import qualified System.IO.Streams as Streams
import System.IO.Streams (InputStream, OutputStream)
import Network.Remote.Resource.Networks() -- ^ import the instance of `Eq`

type HashTable k v = H.BasicHashTable k v

-------------------------------------------------------------------
data MulticastSocket =
  MulticastSocket
    { receiver :: !Socket
    , sender :: !Socket
    , address :: !SockAddr
    }

data SocketStream =
  SocketStream
    { inputStream :: InputStream (ByteString, SockAddr)
    , outputStream :: OutputStream ByteString
    }

data MulticastSocketManager =
  Mgr
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

-- | Get all opened sockets
openedSockets :: MulticastSocketManager -> IO [(NetworkInterface, SocketStream)]
openedSockets = H.toList . _core

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

defaultMulticastSocket :: MulticastSocketManager -> IO SocketStream
defaultMulticastSocket (Mgr (host, port) _) = multicastOn host port Nothing

getWithInterface :: MulticastSocketManager -> NetworkInterface -> IO SocketStream
getWithInterface (Mgr (host, port) var) net = do
  result <- multicastOn host port (Just net)
  H.insert var net result
  return result