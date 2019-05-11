module Network.Remote.Protocol.SimpleStream
  ()where

import Data.Array.IO
import Data.Array.MArray
import Data.Word
import Data.IORef
import Data.Traversable (forM)

data SimpleInputStream=SimpleInputStream {core::IOUArray Int Word8,ptr::IORef Int}

inputstream :: [Word8]->IO SimpleInputStream
inputstream l = do
  arr<-newListArray (0,length l) l
  p <- newIORef 0
  return $ SimpleInputStream arr p

arrayLength ::IOUArray Int Word8->IO Int
arrayLength arr=do
  (a,b)<- getBounds arr
  return (b-a+1)

sRead :: SimpleInputStream -> IO Word8
sRead (SimpleInputStream cor refp) = do
  p <- readIORef refp
  len<-arrayLength cor
  if p<0 || p>= len then
    return 0
  else do
    ru <- readArray cor p
    writeIORef refp (p+1)
    return ru

look :: SimpleInputStream -> IO Word8
look (SimpleInputStream cor refp) = readIORef refp >>= (readArray cor)

skip ::SimpleInputStream -> Int-> IO ()
skip (SimpleInputStream _ refp) k=readIORef refp >>= (writeIORef refp . (+k))

lookRest ::SimpleInputStream -> IO [Word8]
lookRest (SimpleInputStream cor refp) = do
  p <- readIORef refp
  len<-arrayLength cor
  forM [p..len-1] (readArray cor)
