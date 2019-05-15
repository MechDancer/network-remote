module Network.Remote.Resource.Address where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import qualified Data.Map as M
import Network.Remote.Protocol (Name)
import Network.Socket (HostAddress, PortNumber, SockAddr, SockAddr(SockAddrInet), tupleToHostAddress)

newtype Addresses = Ad
  { _core :: MVar (M.Map Name SockAddr)
  }

newAddresses :: IO Addresses
newAddresses = Ad <$> newMVar M.empty

insertSockAddr :: Addresses -> Name -> SockAddr -> IO ()
insertSockAddr (Ad var) name addr = modifyMVar_ var $ \m -> return $ M.insert name addr m

getSockAddr :: Addresses -> Name -> IO (Maybe SockAddr)
getSockAddr (Ad var) name = readMVar var >>= \m -> return $ m M.!? name

insertPort :: Addresses -> Name -> PortNumber -> IO ()
insertPort (Ad var) name port =
  modifyMVar_ var $ \m ->
    return $
    compute m name $ \_ vv ->
      case vv of
        Nothing -> SockAddrInet port $ tupleToHostAddress (0, 0, 0, 0)
        Just (SockAddrInet _ addr) -> SockAddrInet port addr

insertAddr :: Addresses -> Name -> HostAddress -> IO ()
insertAddr (Ad var) name addr =
  modifyMVar_ var $ \m ->
    return $
    compute m name $ \_ vv ->
      case vv of
        Nothing -> SockAddrInet 0 addr
        Just (SockAddrInet port _) -> SockAddrInet port addr

compute :: Ord k => M.Map k v -> k -> (k -> Maybe v -> v) -> M.Map k v
compute m k f = M.insert k (f k $ m M.!? k) m