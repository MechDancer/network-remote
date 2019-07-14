module Network.Remote where

import           Data.Int                (Int64)
import           Data.Time.Clock.System
import           Data.Word               (Word32, Word8)
import           Network.Remote.Protocol

currentTimeSeconds :: IO Int64
currentTimeSeconds = systemSeconds <$> getSystemTime

data MulticastListener =
  ML
    { interest :: [Word8]
    , process  :: RemotePacket -> IO ()
    }

multicastListener :: (Command a) => [a] -> (RemotePacket -> IO ()) -> MulticastListener
multicastListener = ML . map packID
