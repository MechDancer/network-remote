module ZigZag
    (
    showZigZagCodeHex,
    complementEncodeInteger,
    complementDecodeInteger,
    zigZagUIntegerToWordList,
    unZigZagUIntegerFromWordListWC,
    unZigZagUIntegerFromWordListNC,
    unZigZagUIntegerFromWordListNCBE,
    zigZagSplit,
    unZigZagMultipleUInteger
    ) where

import qualified Data.ByteString as B
import Data.Word
import Text.Ascii
import Data.Bits
import Numeric

-- in normal function, it use small endian, the big-endian function will be indicated in the function's name

showZigZagCodeHex :: [Word8] -> String
showZigZagCodeHex as = foldr (\x sf-> (",0x"++) . showHex x . sf) id as ""

takeWhile1 :: (a->Bool)->[a]->[a]
takeWhile1 f a=take (1+ length (takeWhile f a)) a

complementEncodeInteger :: Integer -> Integer
complementEncodeInteger a= if a>=0
  then a*2
  else -2*a-1

complementDecodeInteger :: Integer -> Integer
complementDecodeInteger a= if (a.&.1)==0
  then shift a (negate 1)
  else -(shift (a+1) (negate 1))

-- zigZagUnsignedIntegerToWordList
-- the Integer should be positive, but it do not check it
zigZagUIntegerToWordList :: Integer -> [Word8]
zigZagUIntegerToWordList a=if a>0x7F
  then fromIntegral (a.|.0x80) : (zigZagUIntegerToWordList $ shift a (negate 7))
  else [fromIntegral a]

-- unZigZagUnsignedIntegerFromWordList with check
unZigZagUIntegerFromWordListWC :: [Word8] -> Integer
unZigZagUIntegerFromWordListWC = unZigZagUIntegerFromWordListNC . takeWhile1 (\k->(k.&.0x80)==0x80)

-- unZigZagUnsignedIntegerFromWordList without check
unZigZagUIntegerFromWordListNC :: [Word8] -> Integer
unZigZagUIntegerFromWordListNC = foldr (\a xs-> shift xs 7  + toInteger (a.&.0x7F) ) 0


-- big-endian unZigZagUIntegerFromWordListNC
unZigZagUIntegerFromWordListNCBE :: [Word8] -> Integer
unZigZagUIntegerFromWordListNCBE = foldl (\xs a-> shift xs 7  + toInteger (a.&.0x7F) ) 0

-- split a zigZagList that include mutiple ZigZagNumber
zigZagSplit :: [Word8] -> [[Word8]]
zigZagSplit = reverse . snd . zigZagSplitBase

-- pay attention that buff is big-endian
-- and the result is inverse to the zigZagList's data
-- in order to improve the efficiencyzigZagSplitBase :: [Word8] -> ([Word8],[[Word8]])
zigZagSplitBase = foldl (\(buff,result) x->if (x.&.0x80)==0x80
  then (x:buff,result)  --add x into buffer
  else ([],reverse (x:buff) : result))
  ([],[])

-- unZigZag from a zigZagList that include mutiple ZigZagNumber
unZigZagMultipleUInteger :: [Word8] -> [Integer]
unZigZagMultipleUInteger = reverse . snd . unZigZagMultipleUIntegerBase

-- pay attention that buff is big-endian
-- and the result is inverse to the zigZagList's data
-- in order to improve the efficiency
unZigZagMultipleUIntegerBase :: [Word8] -> ([Word8],[Integer])
unZigZagMultipleUIntegerBase = foldl (\(buff,result) x->if (x.&.0x80)==0x80
  then (x:buff,result)  --add x into buffer
  else ([],unZigZagUIntegerFromWordListNCBE (x:buff) : result))
  ([],[])
