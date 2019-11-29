import           Codec.Binary.UTF8.String              (encodeString)
import           Control.Concurrent                    (forkIO, threadDelay)
import           Control.Monad                         (forever)
import qualified Data.ByteString.Char8                 as B
import           Network.Info                          (ipv4, name)

--import           Network.Mask
--import           Network.Multicast
import           Network.Remote                        (inStr,
                                                        multicastListener)
import           Network.Remote.Protocol               (CommonCmd (..),
                                                        RemotePacket (..))

--import           Network.Remote.Protocol.SimpleStream
--import           Network.Remote.Protocol.SimpleStream.ByteString
import           Network.Remote.Resource.Address
import           Network.Remote.Resource.Networks      (cachedNetwork)
import           Network.Remote.Socket.Broadcaster     (broadcast,
                                                        defaultBroadcasterConfig)
import           Network.Remote.Socket.MulticastSocket
import           Network.Remote.Socket.Receiver        (defaultReceiverConfig,
                                                        runReceiver)

--import           Network.Socket
--import           PID
--import qualified System.IO.Streams                     as S
main :: IO ()
main = do
  manager <- newManager "233.33.33.33" 23333
  addresses <- newAddresses
  -- Open all sockets manually | Errors might be aroused sometimes
--  withManager manager openAllSockets
  let broadcastConfig = defaultBroadcasterConfig (Just "HaskellB") Nothing manager
      receiveConfig = defaultReceiverConfig (Just "HaskellR") Nothing addresses manager
      listener = multicastListener [CommonCmd] print
      recv = runReceiver receiveConfig [listener]
      brod = broadcast broadcastConfig CommonCmd (B.pack . encodeString $ "Hello, Haskell")
  forkIO $ forever recv
  forever brod
