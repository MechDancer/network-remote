module Network.Remote.Socket.Broadcaster
  ( BroadcasterConfig (..),
    defaultBroadcasterConfig,
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
  { name :: !(Maybe String),
    size :: !Int
  }

defaultBroadcasterConfig name Nothing = BroadcasterConfig name 0x4000
defaultBroadcasterConfig name (Just size) = BroadcasterConfig Nothing size

instance Show BroadcasterConfig where
  show (BroadcasterConfig name _) = "BroadcasterConfig[" ++ show name ++ "]"

-- | Broadcast a payload.
-- This does /NOT/ perform 'IO' until fuses with 'MulticastConduit'.
broadcast :: (Monad m, Command c) => BroadcasterConfig -> ConduitT (c, ByteString) ByteString m ()
broadcast (BroadcasterConfig m size) =
  awaitForever $ \(command, payload) -> 
    if isNothing m && (command =.= YELL_ACK || command =.= ADDRESS_ACK)
      then error "No name"
      else yield . B.pack . runConduitPure . (.| sinkList) $ do 
        -- write name
        case m of 
          -- Attention: A @0@ should be appended after the 'String' manully to indicate the ending.
          (Just name) -> yieldMany . C.encode $ name ++ "\0"
          Nothing -> return ()
        -- Write cmd
        yield $ packID command
        -- Write payload
        yieldMany $ B.unpack payload 