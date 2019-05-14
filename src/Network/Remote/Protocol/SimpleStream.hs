module Network.Remote.Protocol.SimpleStream
  ( SimpleInputStream
  , fromList
  , availableIn
  , read
  , readN
  , readZigZag
  , look
  , lookRest
  , SimpleOutputStream
  , empty
  , availableOut
  , write
  , writeList
  , writeToOutputStream
  , toList
  ) where

import Control.Monad (forM_)
import Data.Array.IO (IOUArray, getElems)
import Data.Array.MArray (getBounds, newArray_, newListArray, readArray, writeArray)
import Data.Bits
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Traversable (forM, sequence)
import Data.Word (Word8)
import qualified Network.Remote.Protocol.ZigZag as Z
import Prelude hiding (read)

data SimpleInputStream = SimpleInputStream
  { _coreIn :: IOUArray Int Word8
  , ptrIn :: IORef Int
  }

fromList :: [Word8] -> IO SimpleInputStream
fromList l = do
  arr <- newListArray (0, (length l) - 1) l
  p <- newIORef 0
  return $! SimpleInputStream arr p

availableIn :: SimpleInputStream -> IO Int
availableIn (SimpleInputStream core ptr) = do
  p <- readIORef ptr
  len <- arrayLength core
  return $! len - p

arrayLength :: IOUArray Int Word8 -> IO Int
arrayLength arr = do
  (a, b) <- getBounds arr
  return $! b - a + 1

read :: SimpleInputStream -> IO Word8
read (SimpleInputStream core ptr) = do
  p <- readIORef ptr
  len <- arrayLength core
  if p < 0 || p >= len
    then do
      print "read out of the range"
      return 0
    else do
      ru <- readArray core p
      writeIORef ptr $ p + 1
      return $! ru

readN :: SimpleInputStream -> Int -> IO [Word8]
readN (SimpleInputStream core ptr) k = do
  ava <- availableIn (SimpleInputStream core ptr)
  sequence . take (min ava k) $!
    repeat $ do
      p <- readIORef ptr
      writeIORef ptr $ p + 1
      readArray core p

readZigZag :: SimpleInputStream -> IO Integer
readZigZag stream = Z.decode <$> getZigList
  where
    getZigList = do
      a <- read stream
      if (a .&. 0x80) /= 0x80
        then return [a]
        else do
          rest <- getZigList
          return $! a : rest

look :: SimpleInputStream -> IO Word8
look (SimpleInputStream core ptr) = readIORef ptr >>= readArray core

skip :: SimpleInputStream -> Int -> IO ()
skip (SimpleInputStream _ ptr) k = readIORef ptr >>= writeIORef ptr . (+ k)

lookRest :: SimpleInputStream -> IO [Word8]
lookRest (SimpleInputStream core ptr) = do
  p <- readIORef ptr
  len <- arrayLength core
  forM [p .. len - 1] $! readArray core

-------------------------------------------------------------------
data SimpleOutputStream = SimpleOutputStream
  { _coreOut :: IOUArray Int Word8
  , ptrOut :: IORef Int
  }

empty :: Int -> IO SimpleOutputStream
empty len = do
  arr <- newArray_ (0, len - 1)
  ptr <- newIORef 0
  return $! SimpleOutputStream arr ptr

availableOut :: SimpleOutputStream -> IO Int
availableOut (SimpleOutputStream core ptr) = do
  p <- readIORef ptr
  len <- arrayLength core
  return $! len - p

write :: SimpleOutputStream -> Word8 -> IO ()
write (SimpleOutputStream core ptr) a = do
  p <- readIORef ptr
  len <- arrayLength core
  if p < 0 || p >= len
    then return ()
    else do
      writeArray core p a
      writeIORef ptr $! p + 1

-- if the the list's length greater than available num,it will discard the rest of the list
writeList :: SimpleOutputStream -> [Word8] -> IO ()
writeList o@(SimpleOutputStream core ptr) ax = do
  ava <- availableOut o
  forM_ (take ava ax) $ \a -> do
    p <- readIORef ptr
    writeArray core p a
    writeIORef ptr $! p + 1

writeToOutputStream :: SimpleInputStream -> SimpleOutputStream -> Int -> IO ()
writeToOutputStream i@(SimpleInputStream coreIn ptrIn) o@(SimpleOutputStream coreOut ptrOut) len = do
  avaOut <- availableOut o
  avaIn <- availableIn i
  sequence_ . take (len `min` avaIn `min` avaOut) $
    repeat $ do
      pIn <- readIORef ptrIn
      pOut <- readIORef ptrOut
      readArray coreIn pIn >>= writeArray coreOut pOut
      writeIORef ptrIn $! pIn + 1
      writeIORef ptrOut $! pOut + 1

toList :: SimpleOutputStream -> IO [Word8]
toList (SimpleOutputStream core ptr) = do
  p <- readIORef ptr
  xs <- getElems core
  return $ take (p + 1) xs
