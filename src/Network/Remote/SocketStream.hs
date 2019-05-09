module Network.Remote.SocketStream
()where

import System.IO.Streams.Network
import Network.Remote.MulticastSocket
import qualified Network.Socket.ByteString as B

data SocketStream = SocketStream {inputStream::(InputStream ByteString),outputStream::(OutputStream ByteString)}

multicastSocketToStream :: MulticastSocket -> IO SocketStream
multicastSocketToStream sock = do
  (input , _)<- socketToStream $ receiver sock
  (_, output)<- socketToStream $ sender sock
  return $ SocketStream input output
