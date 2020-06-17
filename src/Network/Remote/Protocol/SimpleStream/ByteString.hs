module Network.Remote.Protocol.SimpleStream.ByteString where

import Codec.Binary.UTF8.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Network.Remote.Protocol.SimpleStream (SimpleInputStream, SimpleOutputStream)
import qualified Network.Remote.Protocol.SimpleStream as S
import qualified Network.Remote.Protocol.ZigZag as ZigZag

-- | Create a `SimpleInputStream` from a `ByteString`
fromByteString :: ByteString -> IO SimpleInputStream
fromByteString = S.fromList . B.unpack

-- | Write a `ByteString` into `SimpleOutputStream` with its length
writeWithLength :: SimpleOutputStream -> ByteString -> IO ()
writeWithLength o pack = do
  S.writeList o $ ZigZag.encode . toInteger . B.length $ pack
  S.writeList o $ B.unpack pack

-- | Read a `Int` and read a `ByteString` whose length equals to it from `SimpleInputStream`
readWithLength :: SimpleInputStream -> IO ByteString
readWithLength i = fmap B.pack $ S.readZigZag i >>= S.readN i . fromIntegral

-- | Write a `String` into `SimpleInputStream` and write `0` to the end
writeEnd :: SimpleOutputStream -> String -> IO ()
writeEnd o s = S.writeList o (encode s) >> S.write o 0

-- | Read and create a `String` from `SimpleInputStream` until meet `0`
readEnd :: SimpleInputStream -> IO String
readEnd i = decode <$> read' []
  where
    read' :: [Word8] -> IO [Word8]
    read' r = do
      next <- S.read i
      if next == 0
        then return r
        else read' (r ++ [next])
