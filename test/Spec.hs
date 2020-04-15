import           Codec.Binary.UTF8.String              (encodeString)
import           Control.Concurrent                    (forkIO, threadDelay)
import           Control.Monad                         (forever)
import qualified Data.ByteString.Char8                 as B
import           Network.Info                          (ipv4, name)
import           Network.Remote.Protocol               (CommonCmd (..),
                                                        RemotePacket (..),
                                                        multicastListener)
import           Network.Remote.Resource.Address
import           Network.Remote.Resource.Networks      (scanNetwork)
import           Network.Remote.Socket.Broadcaster     (broadcast,
                                                        defaultBroadcasterConfig)
import           Network.Remote.Socket.MulticastSocket
import           Network.Remote.Socket.Receiver        (defaultReceiverConfig,
                                                        runReceiver)

main :: IO ()
main = do
  manager <- newManager "233.33.33.33" 23333
  addresses <- newAddresses
  networks <- scanNetwork
  -- Open all sockets manually | Errors might be aroused sometimes
  --  withManager manager openAllSockets
  let broadcastConfig = defaultBroadcasterConfig (Just "HaskellB") Nothing manager
      receiveConfig = defaultReceiverConfig (Just "HaskellR") Nothing addresses networks manager
      listener = multicastListener [CommonCmd] print
      recv = runReceiver receiveConfig [listener]
      brod = broadcast broadcastConfig CommonCmd (B.pack . encodeString $ "Hello, Haskell")
  forkIO $ forever recv
  brod
