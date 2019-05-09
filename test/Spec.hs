import Network.Remote.ZigZag

import Data.IORef
import qualified Data.Map as M
import Network.Remote.MulticastSocket
import Network.Remote
import Data.Time.Clock.System
import Network.Multicast
import Network.Socket
import Control.Monad
import qualified Network.Socket.ByteString as B

main :: IO ()
main = do
--  manager <- newManager "233.33.33.33" 23333
  sock <- multicastReceiver "233.33.33.33" 23333
  forever $ do
    (msg, addr) <- B.recvFrom sock 1024
    print (msg, addr)
