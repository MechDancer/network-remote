module Network.Remote.Socket.Receiver
  ( ReceiverConfig(..)
  , defaultReceiverConfig
  , runReceiver
  ) where

import           Data.Bits
import           Data.ByteString                                 (ByteString)
import qualified Data.ByteString                                 as B
import qualified Data.Foldable                                   as F (find)
import           Data.Maybe                                      (fromJust,
                                                                  isNothing)
import           Network.Info
import           Network.Mask
import           Network.Remote
import           Network.Remote.Protocol
import qualified Network.Remote.Protocol.SimpleStream            as S
import qualified Network.Remote.Protocol.SimpleStream.ByteString as S
import           Network.Remote.Resource.Address
import           Network.Remote.Resource.Networks                (cachedNetwork,
                                                                  split)
import           Network.Remote.Socket.MulticastSocket
import           Network.Socket                                  (SockAddr)
import qualified Network.Socket.ByteString                       as B
import qualified System.IO.Streams                               as Streams

data ReceiverConfig =
  ReceiverConfig
    { _name          :: Maybe String
    , _size          :: Int
    , _addresses     :: Addresses
    , _socketManager :: MulticastSocketManager
    }

defaultReceiverConfig name Nothing = ReceiverConfig name 65536

runReceiver :: ReceiverConfig -> [MulticastListener] -> IO (Maybe RemotePacket)
runReceiver (ReceiverConfig m size addresses manager) listeners = do
  defaultIn <- inputStream <$> withManager manager defaultMulticastSocket
  mPacket <- Streams.read defaultIn
  if isNothing mPacket
    then return Nothing
    else do
      let Just (bs, addr) = mPacket
      i <- S.fromByteString bs
      sender <- S.readEnd i
      -- Packet sent by myself
      if m == Just sender
        then return Nothing
        else do
          cmd <- S.read i
          rest <- S.lookRest i
          withAddresses addresses $ insertSockAddr sender addr
          -- TODO open the corresponding socket of network interface with matched address
          let t = (`F.find` cachedNetwork)
          let u = (`match` addr)
          -- TODO apply u to t
          let filtered = filter (\l -> null (interest l) || cmd `elem` interest l) listeners
              packet = RemotePacket sender cmd (B.pack rest)
          mapM_ (`process` packet) filtered
          return . Just $ packet

match :: NetworkInterface -> SockAddr -> IO Bool
match interface addr =
  (\mask -> networkInterfaceAddrToInt interface .&. mask == sockAddrToInt addr .&. mask) . addressStrToInt <$>
  getSubnetMask interface
  where
    addressStrToInt :: String -> Int
    addressStrToInt s = fromInteger result
      where
        array = split '.' s
        bytes = map read array
        result = foldr (\byte acc -> (acc `shiftL` 8) .|. (byte .&. 0xff)) 0 bytes
    sockAddrToInt :: SockAddr -> Int
    sockAddrToInt = addressStrToInt . show
    networkInterfaceAddrToInt (NetworkInterface _ ipv4 _ _) = addressStrToInt . show $ ipv4
