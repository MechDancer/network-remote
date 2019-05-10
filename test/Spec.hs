import Network.Remote.Protocol.ZigZag

import Control.Monad
import Network.Remote

main :: IO ()
main = do
  scan >>= print
  currentTimeSeconds >>= print
  print $ decodeN . encodeN $ [-1, 2, 3]
  return ()