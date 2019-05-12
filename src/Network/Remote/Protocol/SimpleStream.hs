module Network.Remote.Protocol.SimpleStream
  ( SimpleInputStream
  , sInputStream
  , availableIn
  , read
  , look
  , lookRest
  , SimpleOutputStream
  , sOutputStream
  , availableOut
  , write
  , writeList
  , writeToOutputStream
  ) where

import Control.Monad (forM_)
import Data.Array.IO
import Data.Array.MArray
import Data.IORef
import Data.Traversable (forM, sequence)
import Data.Word
import Prelude hiding (read)

data SimpleInputStream = SimpleInputStream
  { _coreIn :: IOUArray Int Word8
  , ptrIn :: IORef Int
  }

sInputStream :: [Word8] -> IO SimpleInputStream
sInputStream l = do
  arr <- newListArray (0, length l) l
  p <- newIORef 0
  return $ SimpleInputStream arr p

availableIn :: SimpleInputStream -> IO Int
availableIn (SimpleInputStream core ptr) = do
  p <- readIORef ptr
  len <- arrayLength core
  return $ len - p

arrayLength :: IOUArray Int Word8 -> IO Int
arrayLength arr = do
  (a, b) <- getBounds arr
  return $ b - a + 1

read :: SimpleInputStream -> IO Word8
read (SimpleInputStream core ptr) = do
  p <- readIORef ptr
  len <- arrayLength core
  if p < 0 || p >= len
    then return 0
    else do
      ru <- readArray core p
      writeIORef ptr $ p + 1
      return ru

readN :: SimpleInputStream -> Int -> IO [Word8]
readN (SimpleInputStream core ptr) k = do
  ava<- availableIn (SimpleInputStream core ptr)
  sequence . take (min ava k) $ repeat (do
    p<- readIORef ptr
    writeIORef ptr (p+1)
    readArray core p)

look :: SimpleInputStream -> IO Word8
look (SimpleInputStream core ptr) = readIORef ptr >>= readArray core

skip :: SimpleInputStream -> Int -> IO ()
skip (SimpleInputStream _ ptr) k = readIORef ptr >>= writeIORef ptr . (+ k)

lookRest :: SimpleInputStream -> IO [Word8]
lookRest (SimpleInputStream core ptr) = do
  p <- readIORef ptr
  len <- arrayLength core
  forM [p .. len - 1] $ readArray core

-------------------------------------------------------------------
data SimpleOutputStream = SimpleOutputStream
  { _coreOut :: IOUArray Int Word8
  , ptrOut :: IORef Int
  }

sOutputStream :: Int -> IO SimpleOutputStream
sOutputStream len = do
  arr <- newArray_ (0, len - 1)
  ptr <- newIORef 0
  return $ SimpleOutputStream arr ptr

availableOut :: SimpleOutputStream -> IO Int
availableOut (SimpleOutputStream core ptr) = do
  p <- readIORef ptr
  len <- arrayLength core
  return $ len - p

write :: SimpleOutputStream -> Word8 -> IO ()
write (SimpleOutputStream core ptr) a = do
  p <- readIORef ptr
  len <- arrayLength core
  if p < 0 || p >= len
    then return ()
    else do
      writeArray core p a
      writeIORef ptr $ p + 1

-- if the the list's length greater than available num,it will discard the rest of the list
writeList :: SimpleOutputStream -> [Word8] -> IO ()
writeList (SimpleOutputStream core ptr) ax = do
  ava <- availableOut $ SimpleOutputStream core ptr
  forM_ (take ava ax) $ \a -> do
    p <- readIORef ptr
    writeArray core p a
    writeIORef ptr $ p + 1

writeToOutputStream :: SimpleInputStream -> SimpleOutputStream -> Int -> IO ()
writeToOutputStream (SimpleInputStream coreIn ptrIn) (SimpleOutputStream coreOut ptrOut) len = do
  avaOut <- availableOut $ SimpleOutputStream coreOut ptrOut
  avaIn <- availableIn $ SimpleInputStream coreIn ptrIn
  sequence_ . take (len `min` avaIn `min` avaOut) $
    repeat $ do
      pIn <- readIORef ptrIn
      pOut <- readIORef ptrOut
      readArray coreIn pIn >>= writeArray coreOut pOut
      writeIORef ptrIn $ pIn + 1
      writeIORef ptrOut $ pOut + 1
