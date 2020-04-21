module PID where

import qualified Data.Text                                       as T
import           System.IO.Unsafe                                (unsafePerformIO)
import           Text.Parsec
import           Text.Parsec.Text

import qualified Data.ByteString                                 as B
import           Data.IORef                                      (IORef,
                                                                  modifyIORef,
                                                                  newIORef,
                                                                  readIORef)
import           Data.Serialize
import           Network.Remote.Protocol                         (RemotePacket (..))
import           Network.Remote.Protocol.SimpleStream            (SimpleOutputStream,
                                                                  empty)
import           Network.Remote.Protocol.SimpleStream.ByteString
import           Network.Remote.Protocol.SimpleStream.Reader     hiding (read)

data PID =
  PID
    { _kp :: Double
    , _ki :: Double
    , _kd :: Double
    , _ia :: Double
    , _da :: Double
    }
  deriving (Show, Read)


double :: Parser Double
double =
  (do part1 <- many digit
      dot <- char '.'
      part2 <- many digit
      return . read $ part1 ++ [dot] ++ part2) <|>
  (read <$> many digit)

{-# NOINLINE currentPID #-}
currentPID = unsafePerformIO . newIORef $ PID 0 0 0 0 0

term name block = string name >> spaces >> double >>= \num -> return $ modifyIORef currentPID (block num)

kp = term "kp" (\num old -> old {_kp = num})

ki = term "ki" (\num old -> old {_ki = num})

kd = term "kd" (\num old -> old {_kd = num})

ia = term "ia" (\num old -> old {_ia = num})

da = term "da" (\num old -> old {_da = num})

expr = many . choice . map (\it -> spaces >> try it >>= \r -> spaces >> return r) $ [kp, ki, kd, ia, da]

readPID = do
  getLine >>= \line ->
    case runParser expr () "" (T.pack line) of
      Right a -> sequence_ a
      Left e  -> print e
  readIORef currentPID

toRemotePacket = do
  (PID _kp _ki _kd _ia _da) <- readIORef currentPID
  o <- empty 65536
  result <-
    withOutputStream o $ do
      let append = writeList . B.unpack . encode
      write 0
      append _kp
      append _ki
      append _kd
      append _ia
      append _da
      writeList . B.unpack . encode $ True
      toList
  return $ RemotePacket "Haskell" 9 (B.pack result)
