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

kp = string "kp" >> spaces >> double >>= \num -> return $ modifyIORef currentPID (\old -> old {_kp = num})

ki = string "ki" >> spaces >> double >>= \num -> return $ modifyIORef currentPID (\old -> old {_ki = num})

kd = string "kd" >> spaces >> double >>= \num -> return $ modifyIORef currentPID (\old -> old {_kd = num})

ia = string "ia" >> spaces >> double >>= \num -> return $ modifyIORef currentPID (\old -> old {_ia = num})

da = string "da" >> spaces >> double >>= \num -> return $ modifyIORef currentPID (\old -> old {_da = num})

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
      write 0
      writeList . B.unpack . encode $ _kp
      writeList . B.unpack . encode $ _ki
      writeList . B.unpack . encode $ _kd
      writeList . B.unpack . encode $ _ia
      writeList . B.unpack . encode $ _da
      writeList . B.unpack . encode $ True
      toList
  return $ RemotePacket "Haskell" 9 (B.pack result)
