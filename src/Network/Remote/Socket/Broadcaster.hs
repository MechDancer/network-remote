module Network.Remote.Socket.Broadcaster
  ( payloadEncoder,
    broadcast,
  )
where

import Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (forM_)
import Network.Remote.Protocol
import Network.Remote.Protocol.Conduit.ByteString
import Network.Remote.Socket.MulticastSocket
import qualified Network.Socket.ByteString as B

-- | Encode a payload.
-- This does /NOT/ perform 'IO' until fuses with 'MulticastConduit'.
payloadEncoder :: (Monad m, Command c) => TerminalName -> ConduitT (c, ByteString) ByteString m ()
payloadEncoder name =
  awaitForever $ \(command, payload) ->
    yield . runProducer $ do
      -- write name
      yieldStringEnd name
      -- Write cmd
      yield $ packID command
      -- Write payload
      yieldMany $ B.unpack payload

-- | Send packet to all opened sockets.
broadcast :: (MonadIO m, Command c) => TerminalName -> MulticastSocketManager -> ConduitT (c, ByteString) o m ()
broadcast name manager =
  payloadEncoder name
    .| awaitForever
      ( \pack -> do
          socks <- map (\(_, s) -> output . multicastSocketToConduit $ s) <$> withManager manager openedSockets
          forM_ socks (\s -> yield pack .| s)
      )