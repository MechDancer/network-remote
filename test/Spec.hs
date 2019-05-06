import Network.Remote.ZigZag

main :: IO ()
main = print $ decodeM . encodeM $ [-1, 2, 3]