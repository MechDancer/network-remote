module Network.Remote.Socket.Broadcaster
  ( BroadcasterConfig(..)
  , defaultBroadcasterConfig
  , broadcast
  ) where

import           Data.ByteString                                 (ByteString)
import qualified Data.ByteString                                 as B
import           Data.Foldable                                   (forM_)
import           Data.Maybe                                      (isNothing)
import           Network.Remote.Protocol
import qualified Network.Remote.Protocol.SimpleStream            as S
import qualified Network.Remote.Protocol.SimpleStream.ByteString as S
import           Network.Remote.Socket.MulticastSocket
import qualified Network.Socket.ByteString                       as B
import qualified System.IO.Streams                               as Streams

data BroadcasterConfig =
  BroadcasterConfig
    { name          :: Maybe String
    , size          :: Int
    , socketManager :: MulticastSocketManager
    }

defaultBroadcasterConfig name Nothing     = BroadcasterConfig name 0x4000
defaultBroadcasterConfig name (Just size) = BroadcasterConfig Nothing size

instance Show BroadcasterConfig where
  show (BroadcasterConfig name _ _) = "BroadcasterConfig[" ++ show name ++ "]"

-- | Broadcast a payload
broadcast :: (Command a) => BroadcasterConfig -> a -> ByteString -> IO ()
broadcast (BroadcasterConfig m size manager) cmd payload =
  if isNothing m && (cmd =.= YELL_ACK || cmd =.= ADDRESS_ACK)
    then error "No name"
    else do
      stream <- S.empty size
      case m of
        -- Write name
        (Just name) -> S.writeEnd stream name
        Nothing     -> return ()
      -- Write cmd
      S.write stream $ packID cmd
      -- Write payload
      S.writeList stream $ B.unpack payload
      list <- S.toList stream
      socks <- withManager manager openedSockets
      let package = Just $ B.pack list
      forM_ socks (Streams.write package . outputStream . snd)
