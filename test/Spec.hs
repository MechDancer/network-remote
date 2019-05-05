import ZigZag


main :: IO ()
main = do
  let a=zigZagUIntegerToWordList 0xFFFF
  let b=zigZagUIntegerToWordList 0xFFF
  print . fmap unZigZagUIntegerFromWordListNC $ zigZagSplit (a++b)
