module Network.Remote.Protocol.Conduit.ByteString where

import Codec.Binary.UTF8.String as C
import Conduit
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)
import qualified Network.Remote.Protocol.ZigZag as ZigZag

yieldBS :: (Monad m) => ByteString -> ConduitT i Word8 m ()
yieldBS = yieldMany . B.unpack

-- | Unbox a 'ByteString' to 'Word8' stream with its length as prefix.
yieldBSWithLength :: (Monad m) => ByteString -> ConduitT i Word8 m ()
yieldBSWithLength pack = do
  yieldMany $ ZigZag.encode . toInteger . B.length $ pack
  yieldMany $ B.unpack pack

-- | Read a 'Int' as length then read a 'ByteString' with corresponding length from 'Word8' stream.
readWithBSLength :: (Monad m) => ConduitT Word8 o m ByteString
readWithBSLength = do
  n <- fromIntegral . ZigZag.decode <$> (takeWhileC (\a -> a .&. 0x80 /= 0x80) .| sinkList)
  result <- takeC n .| sinkList
  return $ B.pack result

-- | Encode a 'String' to '[Word8]' with @0@ suffix.
yieldStringEnd :: (Monad m) => String -> ConduitT i Word8 m ()
yieldStringEnd str = yieldMany (C.encode str) >> yield 0

-- | Read and decode 'String' until meet @0@ from 'Word8' stream.
readStringEnd :: (Monad m) => ConduitT Word8 o m String
readStringEnd = do
  str <- takeWhileC (/= 0) .| sinkList
  dropC 1
  return $ C.decode str