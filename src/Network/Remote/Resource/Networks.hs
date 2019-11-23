module Network.Remote.Resource.Networks where

import           Control.Monad (forM)
import           Data.Bits
import           Data.Char     (toLower)
import           Data.Hashable
import           Network.Info

scanNetwork :: IO [NetworkInterface]
scanNetwork =
  filter (\i -> foldr (\f acc -> f i && acc) True [isMono, notDocker, notLoopBack, notVMware]) <$> getNetworkInterfaces

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
notDocker = not . inStr "docker" . map toLower . name

notVMware :: NetworkInterface -> Bool
notVMware = not . inStr "VMware" . name

isMono :: NetworkInterface -> Bool
isMono = (\x -> x > 1 && x < 223) . (\(IPv4 a) -> shift a (negate 24)) . ipv4

-------------------------------------------------------------------
split det = wordsWhen (== det)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

inStr a s = foldr (\b bcc -> (a == take (length a) b) || bcc) False (scanr (:) [] s)
