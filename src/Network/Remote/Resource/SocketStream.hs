module Network.Remote.Resource.SocketStream where

import Data.ByteString (ByteString)
import Network.Remote.Resource.MulticastSocket
import qualified Network.Socket.ByteString as B
import System.IO.Streams
import System.IO.Streams.Network

data SocketStream = SocketStream
  { inputStream :: InputStream ByteString
  , outputStream :: OutputStream ByteString
  }

multicastSocketToStream :: MulticastSocket -> IO SocketStream
multicastSocketToStream MulticastSocket {sender = sender, receiver = receiver} = do
  (input, _) <- socketToStreams receiver
  (_, output) <- socketToStreams sender
  return $ SocketStream input output