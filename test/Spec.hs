import Network.Remote.ZigZag

import Control.Concurrent.MVar
import qualified Data.Map as M
import Network.Remote.MulticastSocket

main :: IO ()
main = do
  manager <- newManager "233.33.33.33" 23333
  getWithInterface manager "666.66.66.66"
  newM <- coreM manager
  readMVar newM >>= print . M.keys
  print $ decodeM . encodeM $ [-1, 2, 3]