{-# LANGUAGE RecordWildCards #-}

module Network.Remote.Socket.Receiver
  ( ReceiverConfig (..),
    receivePacket,
    receivePayload,
  )
where

import Conduit
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Conduit.Network.UDP
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Debug.Trace as D
import Network.Info
import Network.Mask
import Network.Remote.Protocol
import Network.Remote.Protocol.Conduit.ByteString
import Network.Remote.Resource.Address
import Network.Remote.Socket.MulticastSocket
import Network.Socket (SockAddr)

data ReceiverConfig = ReceiverConfig
  { name :: !NodeName,
    addresses :: !Addresses,
    networks :: ![NetworkInterface],
    socketManager :: !MulticastSocketManager
  }

-- | Create a multicast receiver stream.
receivePacket :: (MonadIO m) => ReceiverConfig -> ConduitT i RemotePacket m ()
receivePacket ReceiverConfig {..} = do
  MulticastConduit {..} <- withManager socketManager defaultMulticastConduit
  (input .|) $ awaitForever $ \Message {..} ->
    yieldBS msgData .| do
      -- Get sender name
      sender <- readStringEnd
      -- Ignore packet sent by my self
      if sender == name
        then return ()
        else do
          -- Get command
          a <- await
          case a of
            Nothing -> return ()
            (Just cmd) -> do
              -- Get payload
              rest <- sinkList
              -- Update addresses mapping
              withAddresses addresses $ insertSockAddr sender msgSender
              matched <- liftIO $ mapM (\n -> n `match` msgSender >>= \r -> return (n, r)) networks
              -- Open sockets
              withManager socketManager $ openSocket . fst . head $ filter snd matched
              yield $ RemotePacket sender cmd (B.pack rest)

receivePayload :: (MonadIO m, Command c) => ReceiverConfig -> [c] -> ConduitT i ByteString m ()
receivePayload config interests =
  receivePacket config
    .| filterC (\RemotePacket {..} -> command `elem` map packID interests)
    .| mapC (\RemotePacket {..} -> payload)

match :: NetworkInterface -> SockAddr -> IO Bool
match interface addr =
  (\mask -> networkInterfaceAddrToInt interface .&. mask == sockAddrToInt addr .&. mask) . addressStrToInt
    <$> getSubnetMask interface
  where
    addressStrToInt :: String -> Int
    addressStrToInt s = fromInteger result
      where
        -- Drop the port
        array = splitOn "." $ takeWhile (/= '.') s
        bytes = map read array
        result = foldr (\byte acc -> (acc `shiftL` 8) .|. (byte .&. 0xff)) 0 bytes
    sockAddrToInt :: SockAddr -> Int
    sockAddrToInt = addressStrToInt . show
    networkInterfaceAddrToInt (NetworkInterface _ ipv4 _ _) = addressStrToInt . show $ ipv4
