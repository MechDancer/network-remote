module Network.Remote.Protocol.SimpleStream.ByteString where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word8)
import Network.Remote.Protocol.SimpleStream (SimpleInputStream, SimpleOutputStream)
import qualified Network.Remote.Protocol.SimpleStream as S
import qualified Network.Remote.Protocol.ZigZag as ZigZag

fromByteString :: ByteString -> IO SimpleInputStream
fromByteString = S.fromList . B.unpack

writeWithLength :: SimpleOutputStream -> ByteString -> IO ()
writeWithLength o pack = do
  S.writeList o $ ZigZag.encode . toInteger . B.length $ pack
  S.writeList o $ B.unpack pack

readWithLength :: SimpleInputStream -> IO ByteString
readWithLength i = fmap B.pack $ S.readZigZag i >>= S.readN i . fromIntegral

writeEnd :: SimpleOutputStream -> String -> IO ()
writeEnd o s = S.writeList o (B.unpack $ BC.pack s) >> S.write o 0

readEnd :: SimpleInputStream -> IO String
readEnd i = BC.unpack . B.pack <$> read' []
  where
    read' :: [Word8] -> IO [Word8]
    read' r = do
      next <- S.read i
      if next == 0
        then return r
        else read' (r ++ [next])