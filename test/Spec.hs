{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (mapM_)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (foldl1)
import Network.Multicast
import Network.Remote.Resource.Networks (scanNetwork)
import Network.Remote.Socket.MulticastSocket
import Network.Socket
import qualified System.IO.Streams as S

main :: IO ()
main = do
  manager <- newManager "233.233.233.233" 23333
  r <- scanNetwork >>= mapM (\n -> runReaderT (getWithInterface n) manager)
  foldl1 (>>) $ do
    n <- [1 .. 100]
    (SocketStream i o) <- r
    return $ S.write (pure . B.pack $ show n) o
  print "GG"