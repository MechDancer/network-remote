module Network.Remote.Protocol.SimpleStream
  ( SimpleInputStream
  , sInputStream
  , read
  , look
  , lookRest
  ) where

import Data.Array.IO
import Data.Array.MArray
import Data.IORef
import Data.Traversable (forM)
import Data.Word
import Prelude hiding (read)

data SimpleInputStream = SimpleInputStream
  { _core :: IOUArray Int Word8
  , ptr :: IORef Int
  }

sInputStream :: [Word8] -> IO SimpleInputStream
sInputStream l = do
  arr <- newListArray (0, length l) l
  p <- newIORef 0
  return $ SimpleInputStream arr p

arrayLength :: IOUArray Int Word8 -> IO Int
arrayLength arr = do
  (a, b) <- getBounds arr
  return (b - a + 1)

read :: SimpleInputStream -> IO Word8
read (SimpleInputStream core ptr) = do
  p <- readIORef ptr
  len <- arrayLength core
  if p < 0 || p >= len
    then return 0
    else do
      ru <- readArray core p
      writeIORef ptr (p + 1)
      return ru

look :: SimpleInputStream -> IO Word8
look (SimpleInputStream core ptr) = readIORef ptr >>= readArray core

skip :: SimpleInputStream -> Int -> IO ()
skip (SimpleInputStream _ ptr) k = readIORef ptr >>= (writeIORef ptr . (+ k))

lookRest :: SimpleInputStream -> IO [Word8]
lookRest (SimpleInputStream core ptr) = do
  p <- readIORef ptr
  len <- arrayLength core
  forM [p .. len - 1] (readArray core)