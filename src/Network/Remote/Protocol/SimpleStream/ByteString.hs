module Network.Remote.Protocol.SimpleStream.ByteString where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Network.Remote.Protocol.SimpleStream
import qualified Network.Remote.Protocol.ZigZag as ZigZag

writeWithLength :: SimpleOutputStream -> ByteString -> IO ()
writeWithLength o pack = do
  writeList o $ ZigZag.encode . toInteger . B.length $ pack
  writeList o $ B.unpack pack
