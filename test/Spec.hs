import Network.Remote (currentTimeSeconds)
import Network.Remote.Protocol.ZigZag (decodeN, encodeN)
import Network.Remote.Resource.Networks (scan)

main :: IO ()
main = do
  scan >>= print
  currentTimeSeconds >>= print
  print $ decodeN . encodeN $ [-1, 2, 3]
  return ()