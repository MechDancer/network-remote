module Network.Remote.Socket.Broadcaster
  ( BroadcasterConfig (..),
    defaultBroadcasterConfig,
    payloadEncoder,
    broadcast,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (forM_)
import Data.Maybe (isNothing)
import Network.Remote.Protocol
import qualified Network.Remote.Protocol.SimpleStream as S
import qualified Network.Remote.Protocol.SimpleStream.ByteString as S
import Network.Remote.Socket.MulticastSocket
import qualified Network.Socket.ByteString as B
import qualified System.IO.Streams as Streams
import Conduit
import Codec.Binary.UTF8.String as C

data BroadcasterConfig = BroadcasterConfig
  { name :: !String,
    size :: !Int
  }

defaultBroadcasterConfig name = BroadcasterConfig name 0x4000

instance Show BroadcasterConfig where
  show (BroadcasterConfig name _) = "BroadcasterConfig[" ++ show name ++ "]"

-- | Encode a payload.
-- This does /NOT/ perform 'IO' until fuses with 'MulticastConduit'.
payloadEncoder :: (Monad m, Command c) => BroadcasterConfig -> ConduitT (c, ByteString) ByteString m ()
payloadEncoder (BroadcasterConfig name size) =
  awaitForever $ \(command, payload) -> 
    yield . B.pack . runConduitPure . (.| sinkList) $ do 
    -- write name
    -- Attention: A @0@ should be appended after the 'String' manully to indicate the ending.
    yieldMany . C.encode $ name ++ "\0"
    -- Write cmd
    yield $ packID command
    -- Write payload
    yieldMany $ B.unpack payload 

-- | Send packet to all opened socksts.
broadcast :: (MonadIO m, Command c) => BroadcasterConfig -> MulticastSocketManager -> ConduitT (c, ByteString) o m ()
broadcast config manager = payloadEncoder config .| awaitForever (\pack -> do 
  socks <- map (\(_,s) -> output.multicastSocketToConduit $ s) <$> withManager manager openedSockets
  forM_ socks (\s -> yield pack .| s)
  )