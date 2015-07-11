{-# LANGUAGE OverloadedStrings #-}

import Mario
import Types

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Data.Binary 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO


ips = ["127.0.0.1","127.0.0.1"]


main = do gs <- run ips (replicate 4 testGenome)
          print (map (^.fitness) gs)


run :: [String] -> [Genome] -> IO [Genome]
run ips genomes = do
  todo <- newChan :: IO (Chan Genome)
  ntodo <- newMVar (length genomes,todo)
  done <- newChan :: IO (Chan Genome)
  writeList2Chan todo genomes
  addrs <- mapM inet_addr ips
  mapConcurrently (sender ntodo done) addrs
  readNextN done (length genomes)


--TODO exception and error sage
--TODO B.hPutStrLn deprecated?
sender :: MVar (Int,Chan Genome) -> Chan Genome -> HostAddress -> IO ()
sender ntodo done addr = do
  sock <- socket AF_INET Stream 0
  connect sock (SockAddrInet 3000 addr)
  handle <- socketToHandle sock ReadWriteMode
  loop handle
  hClose handle
  where loop :: Handle -> IO ()
        loop handle = do
          (n,todo) <- takeMVar ntodo
          if n < 1
            then let z = strictEncode (0 :: Int)
                 in B.hPutStrLn handle z >> putMVar ntodo (n,todo)
            else do g <- readChan todo
                    putMVar ntodo (n-1,todo)
                    print n
                    let str = strictEncode g
                    let gbytes = strictEncode $ B.length str
                    B.hPutStrLn handle gbytes
                    B.hPut handle str
                    putStrLn $ "sent " ++ show (B.length str)
                    rb <- B.hGetLine handle
                    let rbytes = strictDecode rb
                    resp <- B.hGet handle rbytes
                    putStrLn $ "received " ++ show rbytes
                    result <- strictDecode <$> return resp 
                    writeChan done result
                    loop handle
    

strictEncode :: Binary a => a -> B.ByteString
strictEncode = BL.toStrict . encode
strictDecode :: Binary a => B.ByteString -> a
strictDecode = decode . BL.fromStrict


readNextN :: Chan a -> Int -> IO [a]
readNextN _ 0 = return []
readNextN ch n = readChan ch >>= \x -> (x:) <$> readNextN ch (n - 1)
                   

testGenome :: Genome
testGenome = Genome 0 (om + 1) gs1 where
  im = 169 - 1
  om = 169 + 6
  gs0 = [Gene i o 1 True 0 | i <- [0 .. im], o <- [im + 1 .. om]]
  gs1 = zipWith (set innovation) [0..] gs0
