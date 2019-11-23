{-# LANGUAGE OverloadedStrings #-}

import           Codec.Binary.UTF8.String
import           Control.Monad                         (forM_, mapM_)
import           Control.Monad.Trans.Reader            (runReaderT)
import qualified Data.ByteString.Char8                 as B
import           Data.Foldable                         (foldl1)
import           Network.Info                          (ipv4, name)
import           Network.Mask
import           Network.Multicast
import           Network.Remote                        (inStr)
import           Network.Remote.Protocol               (CommonCmd (..))
import           Network.Remote.Resource.Networks      (scanNetwork)
import           Network.Remote.Socket.Broadcaster     (broadcast,
                                                        defaultBroadcasterConfig)
import           Network.Remote.Socket.MulticastSocket
import           Network.Socket
import qualified System.IO.Streams                     as S

main :: IO ()
main = do
  manager <- newManager "233.33.33.33" 23333
  interface <- head . filter (\n -> inStr "192." (show $ ipv4 n)) <$> scanNetwork
  putStr "Open socket on "
  print interface
  putStr "Subnet mask: "
  getSubnetMask interface >>= print
  -- Open socket manually
  _ <- withManager manager $ openSocket interface
  let config = defaultBroadcasterConfig (Just "Haskell") Nothing manager
  forM_ [1] $ \_ -> broadcast config CommonCmd (B.pack . encodeString $ "Hello, Haskell")
