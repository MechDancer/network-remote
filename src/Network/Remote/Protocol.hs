module Network.Remote.Protocol
  ( CommonCmd (..),
    UdpCmd (..),
    TcpCmd (..),
    Command (..),
    RemotePacket (..),
    remotePacket,
    MulticastListener (..),
    multicastListener,
    (=.=),
    Name,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)

type Name = String

data CommonCmd = CommonCmd deriving (Eq)

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

class (Eq a) => Command a where
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

(=.=) :: (Command a, Command b) => a -> b -> Bool
(=.=) a b = packID a == packID b

data RemotePacket = RemotePacket
  { sender :: !Name,
    command :: !Word8,
    payload :: !ByteString
  }
  deriving (Show)

-- | Build a `RemotePacket`
remotePacket :: (Command m) => Name -> m -> ByteString -> RemotePacket
remotePacket name cmd = RemotePacket name (packID cmd)