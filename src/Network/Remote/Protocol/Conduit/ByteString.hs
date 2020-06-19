module Network.Remote.Protocol.Conduit.ByteString where

import Codec.Binary.UTF8.String
import Conduit
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)
import qualified Network.Remote.Protocol.ZigZag as ZigZag
import Codec.Binary.UTF8.String as C

-- | Prepend the length of a 'ByteString'.
yieldBSWithLength :: (Monad m) =>ByteString -> ConduitT i Word8 m ()
yieldBSWithLength pack = do
  yieldMany $ ZigZag.encode . toInteger . B.length $ pack
  yieldMany $ B.unpack pack

-- | Read a `Int` and read a `ByteString` whose length equals to it from `SimpleInputStream`
readWithBSLength :: (Monad m) => ConduitT Word8 o m ByteString
readWithBSLength = do
  n <- fromIntegral . ZigZag.decode <$> (takeWhileC (\a -> a .&. 0x80 /= 0x80) .| sinkList)
  result <- takeC n .| sinkList
  return $ B.pack result

-- | Encode a 'String' and append @@0@@ to it.
yieldStringEnd :: (Monad m) => String -> ConduitT i Word8 m ()
yieldStringEnd str = yieldMany ((C.encode str) ++ [0::Word8])

-- | Read and create a 'String' until meet @@0@@.
readStringEnd :: (Monad m) => ConduitT Word8 o m String
readStringEnd = do
  str <- takeWhileC (/= 0) .| sinkList
  dropC 1
  return $ C.decode str

readHelper :: (Monad m) => ([Word8] -> a) -> ConduitT ByteString o m (Maybe a)
readHelper go = do
  x <- await
  case x of
    Nothing -> return Nothing
    (Just pack) -> return . return . go . B.unpack $ pack