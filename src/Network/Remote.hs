module Network.Remote
  ( currentTimeSeconds
  , MulticastListener()
  , multicastListener
  , interest
  , process
  , split
  , inStr
  ) where

import           Data.Int                (Int64)
import           Data.Time.Clock.System
import           Data.Word               (Word32, Word8)
import           Network.Remote.Protocol

currentTimeSeconds :: IO Int64
currentTimeSeconds = systemSeconds <$> getSystemTime

data MulticastListener =
  ML
    { interest :: [Word8]
    , process  :: RemotePacket -> IO ()
    }

multicastListener :: (Command a) => [a] -> (RemotePacket -> IO ()) -> MulticastListener
multicastListener = ML . map packID

split det = wordsWhen (== det)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

inStr a s = foldr (\b bcc -> (a == take (length a) b) || bcc) False (scanr (:) [] s)
