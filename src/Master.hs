{-# LANGUAGE OverloadedStrings #-}
module Master where
import Types

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Char8 (hPutStrLn)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO hiding (hPutStrLn)

run :: [Genome] -> IO [Genome]
run genomes = do
  todo <- newChan :: IO (Chan Genome)
  ntodo <- newMVar (length genomes,todo)
  done <- newChan :: IO (Chan Genome)
  writeList2Chan todo genomes
  sender ntodo done
  readNextN done (length genomes)

--TODO exception and error safe
sender :: MVar (Int,Chan Genome) -> Chan Genome -> IO ()
sender ntodo done = withSocketsDo $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 3000 iNADDR_ANY)
  listen sock 5
  loop sock
  where
    loop :: Socket -> IO ()
    loop sock = do
      (sck,_) <- accept sock
      handle <- socketToHandle sck ReadWriteMode
      forkIO $ runLoop handle
      loop sock
    runLoop handle = do
      (n,todo) <- takeMVar ntodo
      if n < 1
        then let z = strictEncode (0 :: Int)
             in hPutStrLn handle z >> putMVar ntodo (n,todo) >> hClose handle
        else do g <- readChan todo
                putMVar ntodo (n-1,todo)
                print n
                result <- sendGenome handle g
                writeChan done result
                runLoop handle


sendGenome :: Handle -> Genome -> IO Genome
sendGenome handle g = do
  let str = strictEncode g
      gbytes = strictEncode $ B.length str
  hPutStrLn handle gbytes
  B.hPut handle str
  putStrLn $ "sent " ++ show (B.length str)
  rb <- B.hGetLine handle
  let rbytes = strictDecode rb
  resp <- B.hGet handle rbytes
  putStrLn $ "received " ++ show rbytes
  return $ strictDecode resp


strictEncode :: Binary a => a -> B.ByteString
strictEncode = BL.toStrict . encode
strictDecode :: Binary a => B.ByteString -> a
strictDecode = decode . BL.fromStrict


readNextN :: Chan a -> Int -> IO [a]
readNextN _ 0 = return []
readNextN ch n = readChan ch >>= \x -> (x:) <$> readNextN ch (n - 1)


testMain = run $ replicate 10 testGenome

testGenome :: Genome
testGenome = Genome 0 (om + 1) gs1 where
  im = 169 - 1
  om = 169 + 6
  gs0 = [Gene i o 1 True 0 | i <- [0 .. im], o <- [im + 1 .. om]]
  gs1 = zipWith (set innovation) [0..] gs0
