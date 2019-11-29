{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Mask
  ( getSubnetMask
  ) where

import           Foreign
import           Foreign.C.String
import           Network.Info     (ipv4)

foreign import ccall unsafe "mask.h c_get_subnet_mask" c_get_subnet_mask :: CString -> CString

getSubnetMask interface = c_get_subnet_mask <$> newCString (show . ipv4 $ interface) >>= peekCString
