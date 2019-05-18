module Network.Remote.Resource.Group
  ( Group()
  , newGroup
  , withGroup
  , detect
  , get
  , getByTimeout
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Data.IORef
import Data.Int (Int64)
import qualified Data.Map as M
import Network.Remote (currentTimeSeconds)
import Network.Remote.Protocol (Name)

newtype Group = Gp
  { _core :: IORef (M.Map String Int64)
  }

newGroup :: IO Group
newGroup = Gp <$> newIORef M.empty

withGroup :: Group -> ReaderT Group m a -> m a
withGroup = flip runReaderT

detect :: (MonadIO m) => Name -> ReaderT Group m (Name, Int64)
detect name =
  ReaderT $ \group ->
    liftIO $ do
      t <- currentTimeSeconds
      modifyIORef (_core group) $ \m -> M.insert name t m
      return (name, t)

get :: (MonadIO m) => Name -> ReaderT Group m (Maybe Int64)
get name =
  ReaderT $ \group ->
    liftIO $ do
      m <- readIORef $ _core group
      return $ M.lookup name m

getByTimeout :: (MonadIO m) => Int64 -> ReaderT Group m [Name]
getByTimeout timeout =
  ReaderT $ \group ->
    liftIO $ do
      now <- currentTimeSeconds
      m <- readIORef $ _core group
      return . M.keys $ M.filterWithKey (\name time -> now - time < timeout) m