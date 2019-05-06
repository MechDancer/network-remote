import Network.Remote.ZigZag

main :: IO ()
main = do
  print . decodeMuti $ encodeMuti [-1,2,3]
