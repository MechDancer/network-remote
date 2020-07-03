{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Painter where

import Conduit
import Data.Bits ((.|.))
import Data.Serialize
import Data.ByteString (ByteString, pack, unpack)
import Data.Word (Word8)
import Network.Remote
import Network.Remote.Protocol.Conduit.ByteString

type Topic = String

type Pack = (Word8, ByteString)

type Vector2D a = (a, a)

type Vector3D a = (a, a, a)

type Number a = (Num a, Serialize a)

paintCommand :: Word8
paintCommand = 6

frameMask :: Word8
frameMask = 10

paintRaw :: Topic -> Word8 -> ByteString -> Pack
paintRaw topic byte bs = (paintCommand,) . pack . runConduitPure . (.| sinkList) $ do
  yieldStringEnd topic
  yield byte
  yieldBS bs

encodeU = unpack . encode

encodeMany :: (Number a) => [a] -> ByteString
encodeMany = foldr ((<>) . encode) ""

paint1 :: (Number a) => Topic -> a -> Pack
paint1 topic x = paintRaw topic 1 $ encode x

paint2 :: (Number a) => Topic -> Vector2D a -> Pack
paint2 topic (x, y) = paintRaw topic 1 $ encodeMany [x, y]

paint3 :: (Number a) => Topic -> Vector3D a -> Pack
paint3 topic (x, y, z) = paintRaw topic 1 $ encodeMany [x, y, z]

paintFrame2 :: (Number a) => Topic -> [[Vector2D a]] -> Pack
paintFrame2 topic list =
  paintRaw topic (2 .|. frameMask) $ pack . runConduitPure . (.| sinkList) $
    yieldMany list
      .| awaitForever
        ( \group -> do
            yieldMany . concatMap (\(x, y) -> unpack . encodeMany $ [x, y]) group
            yieldMany $ encodeU naN
            yieldMany $ encodeU naN
        )

paintFrame3 :: (Number a) => Topic -> [[Vector3D a]] -> Pack
paintFrame3 topic list =
  paintRaw topic (3 .|. frameMask) $ pack . runConduitPure . (.| sinkList) $
    yieldMany list
      .| awaitForever
        ( \group -> do
            yieldMany . concatMap (\(x, y, z) -> unpack . encodeMany $ [x, y, z]) group
            yieldMany $ encodeU naN
            yieldMany $ encodeU naN
        )

naN :: Float
naN = read "NaN"
