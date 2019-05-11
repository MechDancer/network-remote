import Network.Remote (currentTimeSeconds)
import Network.Remote.Protocol.ZigZag (decodeN, encodeN)
import Network.Remote.Resource.Networks (scan)
import Network.Info

main :: IO ()
main = do
  a<-scan
  print a
