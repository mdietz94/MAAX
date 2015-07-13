{-# LANGUAGE OverloadedStrings #-}
module Master where
import Types

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Char8 (hPutStrLn)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO hiding (hPutStrLn)


--to test this load into cabal repl and run "testMain" and start
--the worker in another terminal cabal repl with "main"


run :: [Genome] -> IO [Genome]
run genomes = do
  let ntodo = length genomes
  ndone <- newMVar 0
  todo <- newChan :: IO (Chan Genome)
  done <- newChan :: IO (Chan Genome)
  writeList2Chan todo genomes
  sender ntodo ndone todo done
  readNextN done ntodo


{- ntodo is the number of genomes to be evaluated and should not change
- ndone is the number of genomes completed can goes from 0 to ntodo
- todo is a channel containing all the genomes yet to be done
- done is a channel containing all the genomes completed
-}
sender :: Int -> MVar Int -> Chan Genome -> Chan Genome -> IO ()
sender ntodo ndone todo done = withSocketsDo $ do
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
      --this is needed so sender terminates, when sender is modified
      --so it never closes the socket this will not be needed
      isDone <- (== ntodo) <$> readMVar ndone
      unless isDone (loop sock)
    runLoop :: Handle -> IO ()
    runLoop handle = do
      isDone <- (== ntodo) <$> readMVar ndone
      unless isDone $ do
        g <- readChan todo
        result <- try $ sendGenome handle g
        case result :: Either SomeException Genome of
          Right g' -> writeChan done g' >>
                      incMVar ndone >>
                      runLoop handle
          Left e -> writeChan todo g >> 
                    runLoop handle


incMVar :: MVar Int -> IO ()
incMVar mv = do 
  i <- takeMVar mv
  putMVar mv (i + 1)

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
