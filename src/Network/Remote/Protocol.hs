module Network.Remote.Protocol where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Network.Remote (Name)

data CommonCmd =
  CommonCmd

data UdpCmd
  = YELL_ASK
  | YELL_ACK
  | ADDRESS_ASK
  | ADDRESS_ACK
  | PACKET_SLICE
  | TOPIC_MESSAGE
  deriving (Eq)

data TcpCmd
  = Mail
  | Dialog
  | Blocking
  deriving (Eq)

class Command a where
  packID :: a -> Word8
  -- ^ Get id of the command
  lead :: a -> ByteString -> ByteString
  -- ^ Build a `ByteString` with the command
  lead cmd = B.cons (packID cmd)

instance Command Word8 where
  packID = id

instance Command UdpCmd where
  packID cmd =
    case cmd of
      YELL_ASK -> 0
      YELL_ACK -> 1
      ADDRESS_ASK -> 2
      ADDRESS_ACK -> 3
      PACKET_SLICE -> 4
      TOPIC_MESSAGE -> 5

instance Command TcpCmd where
  packID cmd =
    case cmd of
      Mail -> 0
      Dialog -> 1
      Blocking -> 2

instance Command CommonCmd where
  packID _ = 127

data RemotePacket =
  RPacket
    { sender :: Name
    , command :: Word8
    , payload :: ByteString
    }

-- | Build a `RemotePacket`
remotePacket :: (Command m) => Name -> m -> ByteString -> RemotePacket
remotePacket name cmd = RPacket name (packID cmd)