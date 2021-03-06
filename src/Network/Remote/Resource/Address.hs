module Network.Remote.Resource.Address
  ( Addresses (),
    newAddresses,
    withAddresses,
    insertAddr,
    insertPort,
    insertSockAddr,
    getSockAddr,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), runReaderT)
import qualified Data.Map as M
import Network.Remote.Protocol (TerminalName)
import Network.Socket (HostAddress, PortNumber, SockAddr, SockAddr (SockAddrInet), tupleToHostAddress)

newtype Addresses = Ad
  { _core :: MVar (M.Map TerminalName SockAddr)
  }

newAddresses :: IO Addresses
newAddresses = Ad <$> newMVar M.empty

withAddresses :: Addresses -> ReaderT Addresses m a -> m a
withAddresses = flip runReaderT

insertSockAddr :: (MonadIO m) => TerminalName -> SockAddr -> ReaderT Addresses m ()
insertSockAddr name addr = ReaderT $ \(Ad var) -> liftIO $ modifyMVar_ var $ \m -> return $ M.insert name addr m

getSockAddr :: (MonadIO m) => TerminalName -> ReaderT Addresses m (Maybe SockAddr)
getSockAddr name = ReaderT $ \(Ad var) -> liftIO $readMVar var >>= \m -> return $ m M.!? name

insertPort :: (MonadIO m) => TerminalName -> PortNumber -> ReaderT Addresses m ()
insertPort name port =
  ReaderT $ \(Ad var) ->
    liftIO
      $ modifyMVar_ var
      $ \m ->
        return
          $ compute m name
          $ \_ vv ->
            case vv of
              Nothing -> SockAddrInet port $ tupleToHostAddress (0, 0, 0, 0)
              Just (SockAddrInet _ addr) -> SockAddrInet port addr

insertAddr :: (MonadIO m) => TerminalName -> HostAddress -> ReaderT Addresses m ()
insertAddr name addr =
  ReaderT $ \(Ad var) ->
    liftIO
      $ modifyMVar_ var
      $ \m ->
        return
          $ compute m name
          $ \_ vv ->
            case vv of
              Nothing -> SockAddrInet 0 addr
              Just (SockAddrInet port _) -> SockAddrInet port addr

compute :: Ord k => M.Map k v -> k -> (k -> Maybe v -> v) -> M.Map k v
compute m k f = M.insert k (f k $ m M.!? k) m