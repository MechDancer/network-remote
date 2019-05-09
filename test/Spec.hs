{-# LANGUAGE OverloadedStrings #-}

import Network.Remote.Protocol.ZigZag

import Data.IORef
import qualified Data.Map as M
import Network.Multicast
import Network.Remote.Resource.MulticastSocket
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as N

import qualified Network.Remote.Resource.Networks as N

import Network.Remote

main :: IO ()
main = print "223"
--  currentTimeSeconds >>= print
--  print $ decodeN . encodeN $ [-1, 2, 3]