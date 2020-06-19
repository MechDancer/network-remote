{-# LANGUAGE RecordWildCards #-}
module Network.Remote.Socket.Receiver
  ( ReceiverConfig (..),
    defaultReceiverConfig,
    runReceiver,
  )
where

import Conduit
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B.Internal
import qualified Data.Foldable as F (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing)
import Network.Info
import Network.Mask
import Network.Remote.Protocol
import qualified Network.Remote.Protocol.SimpleStream as S
import qualified Network.Remote.Protocol.SimpleStream.ByteString as S
import Network.Remote.Resource.Address
import Network.Remote.Socket.MulticastSocket
import Network.Socket (SockAddr)
import qualified Network.Socket.ByteString as B
import Data.Conduit.Network.UDP
import Codec.Binary.UTF8.String as C


data ReceiverConfig = ReceiverConfig
  { _name :: !(Maybe String),
    _size :: !Int,
    _addresses :: !Addresses,
    _networks :: ![NetworkInterface],
    _socketManager :: !MulticastSocketManager
  }

defaultReceiverConfig name = ReceiverConfig name 65536

-- | Run multicast receiver
runReceiver :: (MonadIO m) => ReceiverConfig -> ConduitT Message RemotePacket m ()
runReceiver (ReceiverConfig m size addresses networks manager) = 
  awaitForever $ \Message {..} ->
    case (runConduitPure $ (yieldMany . B.unpack $ msgData) .| (do
      -- read name
      sender <- takeWhileC (== B.Internal.c2w '\0') .| sinkList
      -- remove "\0" after name
      dropC 1
       -- Ignore packet sent by myself
      if m == (Just . C.decode $ sender)
        then return ()
      else do
        cmdM <- headC
        case cmdM of 
          Nothing -> return ()
          (Just cmd) -> do
            rest <- sinkList
            yield $ RemotePacket (C.decode sender) cmd (B.pack rest)
    ) .| sinkList) of 
      [] -> return ()
      x:xs -> yield x


  -- defaultIn <- inputStream <$> withManager manager defaultMulticastSocket
  -- mPacket <- Streams.read defaultIn
  -- if isNothing mPacket
  --   then return Nothing
  --   else do
  --     let Just (bs, addr) = mPacket
  --     i <- S.fromByteString bs
  --     -- Read name
  --     sender <- S.readEnd i
  --     -- Ignore packet sent by myself
  --     if m == Just sender
  --       then return Nothing
  --       else-- Read cmd
  --       do
  --         cmd <- S.read i
  --         -- Read payload
  --         rest <- S.lookRest i
  --         -- Update addresses
  --         withAddresses addresses $ insertSockAddr sender addr
  --         matched <- mapM (\n -> n `match` addr >>= \r -> return (n, r)) networks
  --         let correspondingInterface = fst . head $ filter snd matched
  --         -- Open the socket
  --         withManager manager $ openSocket correspondingInterface
  --         let filtered = filter (\l -> null (interest l) || cmd `elem` interest l) listeners
  --             packet = RemotePacket sender cmd (B.pack rest)
  --         -- Handle callbacks
  --         mapM_ (`process` packet) filtered
  --         return . Just $ packet

match :: NetworkInterface -> SockAddr -> IO Bool
match interface addr =
  (\mask -> networkInterfaceAddrToInt interface .&. mask == sockAddrToInt addr .&. mask) . addressStrToInt
    <$> getSubnetMask interface
  where
    addressStrToInt :: String -> Int
    addressStrToInt s = fromInteger result
      where
        array = splitOn "." $ takeWhile (/= '.') s -- Drop port
        bytes = map read array
        result = foldr (\byte acc -> (acc `shiftL` 8) .|. (byte .&. 0xff)) 0 bytes
    sockAddrToInt :: SockAddr -> Int
    sockAddrToInt = addressStrToInt . show
    networkInterfaceAddrToInt (NetworkInterface _ ipv4 _ _) = addressStrToInt . show $ ipv4
