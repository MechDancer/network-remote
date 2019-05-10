module Network.Remote.Resource.Networks where

import Control.Monad (forM)
import Data.Char (toLower)
import Data.IORef
import Network.Info
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE networks #-}
networks :: IORef [NetworkInterface]
networks = unsafePerformIO $ scan >>= newIORef

scan :: IO [NetworkInterface]
scan = do
  interfaces <- getNetworkInterfaces
  result <- newIORef []
  zipped <- newIORef []
  writeIORef result $ do
    f <- [isMono, notDocker, notLoopBack, notVmware]
    list <- forM interfaces $ \x -> return $ f x
    return $ and list
  do r <- readIORef result
     writeIORef zipped $ zip interfaces r
  readIORef zipped >>= writeIORef networks . map fst . filter ((== True) . snd)
  readIORef networks

-------------------------------------------------------------------
instance Eq NetworkInterface where
  a == b = ipv4 a == ipv4 b

instance Ord NetworkInterface where
  a `compare` b = ipv4 a `compare` ipv4 b

-------------------------------------------------------------------
notLoopBack = (/= "127") . take 3 . show . ipv4

notDocker = not . inStr "docker" . map toLower . name

notVmware = not . inStr "VMware" . name

isMono = (\x -> x > 1 && x < 223) . read . take 3 . show . ipv4

-------------------------------------------------------------------
split _ [] = []
split det s =
  let (a, b) = span (/= det) s
   in a :
      split
        det
        (if b /= []
           then tail b
           else [])

inStr a s = foldr (\b bcc -> (a == take (length a) b) || bcc) False (scanr (:) [] s)