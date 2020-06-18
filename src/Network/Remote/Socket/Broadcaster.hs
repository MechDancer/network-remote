{-# LANGUAGE RecordWildCards #-}

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
import Control.Monad.IO.Class
import Conduit

data BroadcasterConfig = BroadcasterConfig
  { name :: !(Maybe String),
    size :: !Int
  }

defaultBroadcasterConfig name Nothing = BroadcasterConfig name 0x4000
defaultBroadcasterConfig name (Just size) = BroadcasterConfig Nothing size

instance Show BroadcasterConfig where
  show (BroadcasterConfig name _) = "BroadcasterConfig[" ++ show name ++ "]"

-- | Broadcast a payload
broadcast :: (MonadIO m) => BroadcasterConfig -> ConduitT RemotePacket ByteString m ()
broadcast (BroadcasterConfig m size) =
  awaitForever $ \RemotePacket {..} -> 
    if isNothing m && (command =.= YELL_ACK || command =.= ADDRESS_ACK)
      then error "No name"
      else
        ( liftIO $ do
            stream <- S.empty size
            -- Write name
            case m of
              (Just name) -> S.writeEnd stream name
              Nothing -> return ()
            -- Write cmd
            S.write stream $ packID command
            -- Write payload
            S.writeList stream $ B.unpack payload
            S.toList stream
        )
          >>= yield
          . B.pack
