module Network.Remote.Resource.Networks where

import Control.Monad (forM)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.Info
import Data.Bits
-- import System.IO.Unsafe (unsafePerformIO)

-- {-# NOINLINE networks #-}
-- networks :: IORef [NetworkInterface]
-- networks = unsafePerformIO $ newIORef []

scan :: IO [NetworkInterface]
scan = do
  interfaces <- getNetworkInterfaces
  return $ filter (\inface->foldr (\f acc->f inface && acc) True [isMono, notDocker, notLoopBack, notVmware] ) interfaces

-------------------------------------------------------------------
instance Eq NetworkInterface where
  a == b = ipv4 a == ipv4 b

instance Ord NetworkInterface where
  a `compare` b = ipv4 a `compare` ipv4 b

-------------------------------------------------------------------

notLoopBack :: NetworkInterface->Bool
notLoopBack = (/= "127") . take 3 . show . ipv4

notDocker :: NetworkInterface->Bool
notDocker = not . inStr "docker" . map toLower . name

notVmware :: NetworkInterface->Bool
notVmware = not . inStr "VMware" . name

isMono :: NetworkInterface->Bool
isMono = (\x -> x > 1 && x < 223) . (\(IPv4 a) -> shift a (negate 24)) . ipv4

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
