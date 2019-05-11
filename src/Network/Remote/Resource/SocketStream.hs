module Network.Remote.Resource.SocketStream
  ( SocketStream(..)
  , multicastSocketToStream
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Internal as S
import Network.Remote.Resource.MulticastSocket
import Network.Socket (SockAddr)
import qualified Network.Socket.ByteString as B
import qualified System.IO.Streams as Streams
import System.IO.Streams (InputStream, OutputStream)

data SocketStream = SocketStream
  { inputStream :: InputStream (ByteString, SockAddr)
  , outputStream :: OutputStream ByteString
  }

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