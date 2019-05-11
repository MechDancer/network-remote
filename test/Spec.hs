{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever, join)
import qualified Data.ByteString.Char8 as B
import Network.Multicast
import Network.Remote.Resource.MulticastSocket
import Network.Remote.Resource.Networks (scanNetwork)
import Network.Remote.Resource.SocketStream
import Network.Socket
import qualified System.IO.Streams as S

main :: IO ()
main = do
  manager <- newManager "233.233.233.233" 23333
  stream <- join $ head <$> scanNetwork >>= (fmap multicastSocketToStream . getWithInterface manager)
  S.write (pure $ B.pack "sdsdsd") $ outputStream stream
  S.read (inputStream stream) >>= print
  print "GG"