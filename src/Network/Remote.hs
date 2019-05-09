module Network.Remote where

import Data.Int (Int64)
import Data.Time.Clock.System
import Data.Word (Word32)

type Name = String

currentTimeSeconds :: IO Int64
currentTimeSeconds = systemSeconds <$> getSystemTime