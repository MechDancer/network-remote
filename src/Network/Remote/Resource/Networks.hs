module Network.Remote.Resource.Networks where

import Data.Char(toLower)

notLoopBack = (/= "127") . take 3

notDocker = not . inStr "docker" . map toLower

notVmware = not . inStr "vmware" . map toLower



split _ [] = []
split det s =
  let (a, b) = span (/= det) s
   in a :
      split
        det
        (if b /= []
           then tail b
           else [])

inStr a s = foldr (\b bcc -> (a == take (length a) b) || bcc) False (scanr (:) [] s)