module Network.Remote.Protocol.SimpleStream.Reader
    (
    ) where
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Network.Remote.Protocol.SimpleStream as S
import Data.Word

withInputStreamDo :: SimpleInputStream -> ReaderT SimpleInputStream m a -> m a
withInputStreamDo = flip runReaderT

availableIn :: (MonadIO m)=> ReaderT SimpleInputStream m Int
availableIn =ReaderT $ liftIO . S.availableIn


read :: (MonadIO m)=> ReaderT SimpleInputStream m Word8
read =ReaderT $ liftIO. S.read

readN :: (MonadIO m)=>Int-> ReaderT SimpleInputStream m [Word8]
readN  k = ReaderT $ liftIO. flip S.readN k

readZigZag :: (MonadIO m)=> ReaderT SimpleInputStream m Integer
readZigZag = ReaderT $ liftIO. S.readZigZag


look :: (MonadIO m)=>ReaderT SimpleInputStream m Word8
look  = ReaderT $ liftIO. S.look

skip :: (MonadIO m)=>Int-> ReaderT SimpleInputStream m ()
skip  k = ReaderT $ liftIO . flip S.skip k

lookRest :: (MonadIO m)=>ReaderT SimpleInputStream m [Word8]
lookRest = ReaderT $ liftIO. S.lookRest

withOutputStreamDo :: SimpleOutputStream -> ReaderT SimpleOutputStream m a -> m a
withOutputStreamDo = flip runReaderT


availableOut :: (MonadIO m)=> ReaderT SimpleOutputStream m Int
availableOut = ReaderT $ liftIO . S.availableOut

write :: (MonadIO m)=> Word8 -> ReaderT SimpleOutputStream m ()
write k = ReaderT $ liftIO . flip S.write k

writeList :: (MonadIO m)=> [Word8] -> ReaderT SimpleOutputStream m ()
writeList l = ReaderT $ liftIO . flip S.writeList l

toList :: (MonadIO m)=> ReaderT SimpleOutputStream m [Word8]
toList = ReaderT $ liftIO . S.toList
