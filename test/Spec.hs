import Network.Remote.ZigZag

import Data.IORef
import qualified Data.Map as M
import Network.Remote.MulticastSocket
import Network.Remote

main :: IO ()
main = do
--  manager <- newManager "233.33.33.33" 23333
  currentTimeSeconds >>= print
  print $ decodeM . encodeM $ [-1, 2, 3]