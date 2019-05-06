module Network.Remote.ZigZag
  ( ZigZagList
  , showZigZagCodeHex
  , encode
  , decode
  , encodeM
  , decodeM
  ) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Word
import Numeric
import Text.Ascii

type ZigZagList = [Word8]

-- In normal function, it use small endian, the big-endian function will be indicated in the function's name
showZigZagCodeHex :: ZigZagList -> String
showZigZagCodeHex as = foldr (\x sf -> (",0x" ++) . showHex x . sf) id as ""

takeWhile1' :: (a -> Bool) -> [a] -> [a]
takeWhile1' f a = take (1 + length (takeWhile f a)) a

signedToUnsigned :: Integer -> Integer
signedToUnsigned a =
  if a >= 0
    then a * 2
    else -2 * a - 1

unsignedToSigned :: Integer -> Integer
unsignedToSigned a =
  if (a .&. 1) == 0
    then shift a (negate 1)
    else -(shift (a + 1) (negate 1))

-- | `zigZagUnsignedIntegerToWordList` encodes a `Integer` to a `ZigZagList`
-- The Integer should be positive, and we do not check it.
encodeUnsigned :: Integer -> ZigZagList
encodeUnsigned a =
  if a > 0x7F
    then fromIntegral (a .|. 128) : encodeUnsigned (shift a (negate 7))
    else [fromIntegral a]

-- | `unZigZagUnsignedIntegerFromWordList` decodes `Integer` from `ZigZagList` with check
unZigZagUIntegerFromWordListWC :: ZigZagList -> Integer
unZigZagUIntegerFromWordListWC = unZigZagUIntegerFromWordListNC . takeWhile1' (\k -> (k .&. 0x80) == 0x80)

-- | `unZigZagUnsignedIntegerFromWordList` decodes `Integer` from`ZigZagList` without check
unZigZagUIntegerFromWordListNC :: ZigZagList -> Integer
unZigZagUIntegerFromWordListNC = foldr (\a xs -> shift xs 7 + toInteger (a .&. 0x7F)) 0

-- | `unZigZagUIntegerFromWordListNC`  decodes `Integer` from`ZigZagList` (big-endian)
unZigZagUIntegerFromWordListNCBE :: ZigZagList -> Integer
unZigZagUIntegerFromWordListNCBE = foldl (\xs a -> shift xs 7 + toInteger (a .&. 0x7F)) 0

-- | Splits a ZigZag list including multiple ZigZagNumbers
zigZagSplit :: ZigZagList -> [ZigZagList]
zigZagSplit = reverse . snd . zigZagSplitBase

-- pay attention that the buff is big-endian
-- and the result is inverse to the zigZagList's data
-- in order to improve the efficiency
zigZagSplitBase :: ZigZagList -> (ZigZagList, [ZigZagList])
zigZagSplitBase =
  foldl
    (\(buff, result) x ->
       if (x .&. 0x80) == 0x80
         then (x : buff, result) --add x into buffer
         else ([], reverse (x : buff) : result))
    ([], [])

-- | UnZigZag from a zigZagList including multiple ZigZagNumbers
unZigZagMultipleUInteger :: ZigZagList -> [Integer]
unZigZagMultipleUInteger = reverse . snd . unZigZagMultipleUIntegerBase

-- pay attention that the buff is big-endian
-- and the result is inverse to the zigZagList's data
-- in order to improve the efficiency
unZigZagMultipleUIntegerBase :: ZigZagList -> (ZigZagList, [Integer])
unZigZagMultipleUIntegerBase =
  foldl
    (\(buff, result) x ->
       if (x .&. 0x80) == 0x80
         then (x : buff, result) --add x into buffer
         else ([], unZigZagUIntegerFromWordListNCBE (x : buff) : result))
    ([], [])

-- | encode single Integer
encode :: Integer -> ZigZagList
encode = encodeUnsigned . signedToUnsigned

decode :: ZigZagList -> Integer
decode = unsignedToSigned . unZigZagUIntegerFromWordListWC

encodeM :: [Integer] -> ZigZagList
encodeM = foldr (\int list -> encode int ++ list) []

decodeM :: ZigZagList -> [Integer]
decodeM = fmap unsignedToSigned . unZigZagMultipleUInteger