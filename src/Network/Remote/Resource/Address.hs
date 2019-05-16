module Network.Remote.Resource.Address where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.Map as M
import Network.Remote.Protocol (Name)
import Network.Socket (HostAddress, PortNumber, SockAddr, SockAddr(SockAddrInet), tupleToHostAddress)

newtype Addresses = Ad
  { _core :: MVar (M.Map Name SockAddr)
  }

newAddresses :: IO Addresses
newAddresses = Ad <$> newMVar M.empty

insertSockAddr :: (MonadIO m) => Name -> SockAddr -> ReaderT Addresses m ()
insertSockAddr name addr = ReaderT $ \(Ad var) -> liftIO $ modifyMVar_ var $ \m -> return $ M.insert name addr m

getSockAddr :: (MonadIO m) => Name -> ReaderT Addresses m (Maybe SockAddr)
getSockAddr name = ReaderT $ \(Ad var) -> liftIO $readMVar var >>= \m -> return $ m M.!? name

insertPort :: (MonadIO m) => Name -> PortNumber -> ReaderT Addresses m ()
insertPort name port =
  ReaderT $ \(Ad var) ->
    liftIO $
    modifyMVar_ var $ \m ->
      return $
      compute m name $ \_ vv ->
        case vv of
          Nothing -> SockAddrInet port $ tupleToHostAddress (0, 0, 0, 0)
          Just (SockAddrInet _ addr) -> SockAddrInet port addr

insertAddr :: (MonadIO m) => Name -> HostAddress -> ReaderT Addresses m ()
insertAddr name addr =
  ReaderT $ \(Ad var) ->
    liftIO $
    modifyMVar_ var $ \m ->
      return $
      compute m name $ \_ vv ->
        case vv of
          Nothing -> SockAddrInet 0 addr
          Just (SockAddrInet port _) -> SockAddrInet port addr

compute :: Ord k => M.Map k v -> k -> (k -> Maybe v -> v) -> M.Map k v
compute m k f = M.insert k (f k $ m M.!? k) m