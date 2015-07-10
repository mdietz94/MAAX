{-# LANGUAGE OverloadedStrings #-}

import Mario
import Types

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Data.Binary
import qualified Data.ByteString.Lazy as B
import Network.Socket
import System.IO

main = do
  addr <- inet_addr "127.0.0.1"
  i <- sendGenome addr 0 testGenome
  return ()


sendGenome :: HostAddress -> Int -> Genome -> IO Genome
sendGenome addr uid g = do
  sock <- socket AF_INET Stream 0
  connect sock (SockAddrInet 3000 addr)
  handle <- socketToHandle sock ReadWriteMode
  B.hPut handle (encode g)
  result <- decode <$> B.hGetContents handle :: IO Genome
  print result
  hClose handle
  return result


runMaster :: [HostAddress] -> [(Int,Genome)] -> IO [Int]
runMaster _ [] = return []
runMaster workers gs = do
  fs <- mapConcurrently (\(w,(uid,g)) -> sendGenome w uid g) (zip workers gs)
  runMaster workers (drop (length workers) gs)

testGenome :: Genome
testGenome = Genome 0 (om + 1) gs1 where
  im = 169 - 1
  om = 169 + 6
  gs0 = [Gene i o 1 True 0 | i <- [0 .. im], o <- [im + 1 .. om]]
  gs1 = zipWith (set innovation) [0..] gs0
