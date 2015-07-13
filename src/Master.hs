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
import System.Directory
import Mario
import NeuralNetwork hiding (run)
import Emulator (saveAsFM2)
import System.Random

--to test this load into cabal repl and run "testMain" and start
--the worker in another terminal cabal repl with "main"

replaceGenomes :: Population -> [Genome] -> Population
replaceGenomes pop [] = pop
replaceGenomes pop xs = do
  (a,b,c,d,gs) <- pop
  (a,b,c,d,take (length gs) xs) : replaceGenomes pop (drop (length gs) xs)

runPopulation (gInnov,p0,gen) n sock = do
  let genomes = concatMap (\(_,_,_,_,gs) -> gs) p0
  genomes' <- run genomes sock
  let p1 = replaceGenomes p0 genomes'
  let (gInnov',p2,gen') = stepNetwork p0 (gInnov,p1,gen) marioConfig
  savePopulation ("./data/" ++ (show (n+1)) ++ ".bin") p2
  joydata <- recordMario (fittestGenome p2)
  saveAsFM2 ("./data/" ++ (show (n+1)) ++ ".fm2") joydata
  runPopulation (gInnov',p2,gen') (n+1) sock

loadMaxPopulation = loadMaxPop 1
  where
    loadMaxPop n = do
      exists <- doesFileExist $ "./data/" ++ show n ++ ".bin"
      if exists
         then loadMaxPop (n+1)
         else do
           p0 <- loadPopulation $ "./data/" ++ show (n-1) ++ ".bin"
           return (n-1,p0)

main = withSocketsDo $ do
  (num,p0) <- loadMaxPopulation
  let genomes = concatMap (\(_,_,_,_,gs) -> gs) p0
  let gInnov = maximum . map _innovation . concatMap _genes $ genomes
  let gen = randomRs (0.0,1.0) $ mkStdGen 23
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 3000 iNADDR_ANY)
  listen sock 5
  putStrLn "Listening on port 3000"
  runPopulation (gInnov,p0,gen) num sock

run :: [Genome] -> Socket -> IO [Genome]
run genomes sock = do
  let ntodo = length genomes
  ndone <- newMVar 0
  todo <- newChan :: IO (Chan Genome)
  done <- newChan :: IO (Chan Genome)
  writeList2Chan todo genomes
  sender ntodo ndone todo done sock
  readNextN done ntodo


{- ntodo is the number of genomes to be evaluated and should not change
- ndone is the number of genomes completed can goes from 0 to ntodo
- todo is a channel containing all the genomes yet to be done
- done is a channel containing all the genomes completed
-}
sender :: Int -> MVar Int -> Chan Genome -> Chan Genome -> Socket -> IO ()
sender ntodo ndone todo done sock = withSocketsDo $ loop sock
  where
    loop :: Socket -> IO ()
    loop sock = do
      (sck,_) <- accept sock
      putStrLn "Connection accepted!"
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
