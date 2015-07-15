{-# LANGUAGE OverloadedStrings #-}
import Types
import Mario
import NeuralNetwork hiding (run)
import Emulator (saveAsFM2)

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Char8 (hPutStrLn)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.IO hiding (hPutStrLn)
import System.Directory
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Random


--to test this load into cabal repl and run "testMain" and start
--the worker in another terminal cabal repl with "main"

replaceGenomes :: Population -> [Genome] -> Population
replaceGenomes [] _   = []
replaceGenomes _ [] = []
replaceGenomes ((a,b,c,d,gs):pop) xs = (a,b,c,d, take (length gs) xs) : replaceGenomes pop (drop (length gs) xs)

runPopulation :: (Int, Population, [Float]) -> Int -> Socket -> IO b
runPopulation (gInnov,p0,gen) n sock = do
  let genomes = concatMap (\(_,_,_,_,gs) -> gs) p0
  genomes' <- run genomes sock
  let p1 = replaceGenomes p0 genomes'
  savePopulation ("./data/" ++ show n ++ ".bin") p1
  let (gInnov',p2,gen') = stepNetwork p0 (gInnov,p1,gen) marioConfig
  savePopulation ("./data/" ++ show (n+1) ++ ".bin") p2
  joydata <- recordMario (fittestGenome p2)
  saveAsFM2 ("./data/" ++ show (n+1) ++ ".fm2") joydata
  runPopulation (gInnov',p2,gen') (n+1) sock

loadMaxPopulation :: IO (Int, Population)
loadMaxPopulation = loadMaxPop 1
  where
    loadMaxPop n = do
      exists <- doesFileExist $ "./data/" ++ show n ++ ".bin"
      if exists
         then loadMaxPop (n+1)
         else do
           p0 <- loadPopulation $ "./data/" ++ show (n-1) ++ ".bin"
           return (n-1,p0)

main :: IO ()
main = withSocketsDo $ do
  logH <- fileHandler "Master.log" INFO >>= \lh -> return $
          setFormatter lh (simpleLogFormatter "[$time : $prio] $msg")
  updateGlobalLogger rootLoggerName (addHandler logH)
  (num,p0) <- loadMaxPopulation
  let genomes = concatMap (\(_,_,_,_,gs) -> gs) p0
  let gInnov = maximum . map _innovation . concatMap _genes $ genomes
  let gen = randomRs (0.0,1.0) $ mkStdGen 23
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 3000 iNADDR_ANY)
  listen sock 5
  infoM rootLoggerName "Listening on port 3000"
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
sender ntodo ndone todo done sock0 = withSocketsDo $ loop sock0
  where
    loop :: Socket -> IO ()
    loop sock = do
      (sck,_) <- accept sock
      infoM rootLoggerName "Connection accepted!"
      sHandle <- socketToHandle sck ReadWriteMode
      _ <- forkIO $ runLoop sHandle
      --this is needed so sender terminates, when sender is modified
      --so it never closes the socket this will not be needed
      isDone <- (== ntodo) <$> readMVar ndone
      unless isDone (loop sock)
    runLoop :: Handle -> IO ()
    runLoop sHandle = do
      isDone <- (== ntodo) <$> readMVar ndone
      unless isDone $ do
        g <- readChan todo
        result <- try $ sendGenome sHandle g
        case result :: Either SomeException Genome of
          Right g' -> writeChan done g' >>
                      incMVar ndone >>
                      runLoop sHandle
          Left _ -> writeChan todo g >>
                    runLoop sHandle


incMVar :: MVar Int -> IO ()
incMVar mv = do
  i <- takeMVar mv
  putMVar mv (i + 1)

sendGenome :: Handle -> Genome -> IO Genome
sendGenome sHandle g = do
  let str = strictEncode g
      gbytes = strictEncode $ B.length str
  hPutStrLn sHandle gbytes
  B.hPut sHandle str
  putStrLn $ "sent " ++ show (B.length str)
  rb <- B.hGetLine sHandle
  let rbytes = strictDecode rb
  resp <- B.hGet sHandle rbytes
  putStrLn $ "received " ++ show rbytes
  return $ strictDecode resp


strictEncode :: Binary a => a -> B.ByteString
strictEncode = BL.toStrict . encode
strictDecode :: Binary a => B.ByteString -> a
strictDecode = decode . BL.fromStrict


readNextN :: Chan a -> Int -> IO [a]
readNextN _ 0 = return []
readNextN ch n = readChan ch >>= \x -> (x:) <$> readNextN ch (n - 1)
