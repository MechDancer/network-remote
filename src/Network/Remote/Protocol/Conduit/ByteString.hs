module Network.Remote.Protocol.Conduit.ByteString where

import Codec.Binary.UTF8.String
import Conduit
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)
import qualified Network.Remote.Protocol.ZigZag as ZigZag

-- | Prepend the length of a 'ByteString'.
yieldBSWithLength :: (Monad m) => ConduitT ByteString ByteString m ()
yieldBSWithLength = mapC $ \pack -> B.pack . ZigZag.encode . toInteger . B.length $ pack <> pack

-- | Read a `Int` and read a `ByteString` whose length equals to it from `SimpleInputStream`
readWithLength :: (Monad m) => ConduitT ByteString o m (Maybe ByteString)
readWithLength = readHelper go
  where
    go :: [Word8] -> ByteString
    go list = runConduitPure $
      (yieldMany list) .| do
        n <- fromIntegral . ZigZag.decode <$> (takeWhileC (\a -> a .&. 0x80 /= 0x80) .| sinkList)
        result <- takeC n .| sinkList
        return $ B.pack result

-- | Encode a 'String' and append @@0@@ to it.
yieldStringEnd :: (Monad m) => ConduitT String String m ()
yieldStringEnd = mapC $ \s -> encodeString s ++ ['0']

-- | Read and create a 'String' until meet @@0@@.
readStringEnd :: (Monad m) => ConduitT ByteString o m (Maybe String)
readStringEnd = readHelper go
  where
    go :: [Word8] -> String
    go list = decode . runConduitPure $ (yieldMany list) .| takeWhileC (/= 0) .| sinkList

readHelper :: (Monad m) => ([Word8] -> a) -> ConduitT ByteString o m (Maybe a)
readHelper go = do
  x <- await
  case x of
    Nothing -> return Nothing
    (Just pack) -> return . return . go . B.unpack $ pack