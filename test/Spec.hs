import Network.Remote.Protocol.ZigZag

import Data.IORef
import qualified Data.Map as M
import Network.Remote.Resource.MulticastSocket

import Network.Remote

main :: IO ()
main = do
--  manager <- newManager "233.33.33.33" 23333
  currentTimeSeconds >>= print
  print $ decodeN . encodeN $ [-1, 2, 3]