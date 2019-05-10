module Network.Remote.Resource.Group
  ( Group
  , newGroup
  , detect
  , get
  , getByTimeout
  ) where

import Data.IORef
import Data.Int (Int64)
import qualified Data.Map as M
import Network.Remote (Name, currentTimeSeconds)

newtype Group = Gp
  { _core :: IORef (M.Map String Int64)
  }

newGroup :: IO Group
newGroup = newIORef M.empty >>= \core -> return $ Gp core

detect :: Group -> Name -> IO (Name, Int64)
detect group name = do
  t <- currentTimeSeconds
  modifyIORef (_core group) $ \m -> M.insert name t m
  return (name, t)

get :: Group -> Name -> IO (Maybe Int64)
get group name = do
  m <- readIORef $ _core group
  return $ M.lookup name m

getByTimeout :: Group -> Int64 -> IO [Name]
getByTimeout group timeout = do
  now <- currentTimeSeconds
  m <- readIORef $ _core group
  return . M.keys $ M.filterWithKey (\name time -> now - time < timeout) m