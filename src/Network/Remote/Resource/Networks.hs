module Network.Remote.Resource.Networks (scanNetwork) where

import Control.Monad (forM)
import Data.Bits
import Data.Char (toLower)
import Data.Hashable
import Data.List (isInfixOf)
import Network.Info
import System.IO.Unsafe (unsafePerformIO)

-- | Scan network interfaces right now
scanNetwork :: IO [NetworkInterface]
scanNetwork =
  filter (\i -> foldr (\f acc -> f i && acc) True [isMono, notDocker, notLoopBack, notVMware, noNull]) <$> getNetworkInterfaces

-------------------------------------------------------------------
instance Eq NetworkInterface where
  a == b = ipv4 a == ipv4 b

instance Ord NetworkInterface where
  a `compare` b = ipv4 a `compare` ipv4 b

instance Hashable NetworkInterface where
  hashWithSalt a net =
    let (IPv4 w) = ipv4 net
     in hashWithSalt a w

-------------------------------------------------------------------
notLoopBack :: NetworkInterface -> Bool
notLoopBack = (/= "127") . take 3 . show . ipv4

notDocker :: NetworkInterface -> Bool
notDocker = not . isInfixOf "docker" . map toLower . name

notVMware :: NetworkInterface -> Bool
notVMware = not . isInfixOf "VMware" . name

isMono :: NetworkInterface -> Bool
isMono = (\x -> x > 1 && x < 223) . (\(IPv4 a) -> shift a (negate 24)) . ipv4

noNull :: NetworkInterface -> Bool
noNull = (/= "0.0.0.0") . show . ipv4
