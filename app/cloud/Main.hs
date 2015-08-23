{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
module Main where

import Types
import qualified Mario as M

import qualified Control.Concurrent as C
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Control.Monad.Loops
import Data.List (sortOn)
import System.Random.Mersenne.Pure64
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Text.Printf
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import System.Environment

distribMain :: ([NodeId] -> Process ()) -> (RemoteTable -> RemoteTable) -> IO ()
distribMain master frtable = do
  args <- getArgs
  let rtable = frtable initRemoteTable
      defaultHost = "localhost"
      defaultPort = "44444"
  case args of
    [] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend master
    [ "master" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend master
    [ "master", host] -> do
      backend <- initializeBackend host defaultPort rtable
      startMaster backend master
    [ "master", host, port ] -> do
      backend <- initializeBackend host port rtable
      startMaster backend master
    [ "slave" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startSlave backend
    [ "slave", port ] -> do
      backend <- initializeBackend defaultHost port rtable
      startSlave backend
    [ "slave", host, port ] -> do
      backend <- initializeBackend host port rtable
      startSlave backend
    _ -> print "Usage: ray-tracer-cloud <master|slave> [ <ip_address> <port> ]"


data Message = MsgGenome ProcessId Genome
             | MsgAck ProcessId
  deriving (Typeable, Generic)

instance Binary Message

pingServer :: Process ()
pingServer = do
  MsgWorld from0 world <- expect
  mypid <- getSelfPid
  send from0 (MsgAck mypid)
  rng <- liftIO newPureMT
  let grids = generateGrids rng (round (wImgWd world) + 10) (wAntiAliasing world)
  forever $ do
    MsgWork from (i,wID,start,step) <- expect
    let img = renderIxs grids world start step
    send from (MsgImg mypid (i,wID,img))
    say $ printf "completed %d(%d-%d)" wID start (start + step)

remotable ['pingServer]

{- The use of MVars in this function is NOT thread safe. The only reason they're
- is because legacy and I'm too lazy to change them to a mutable value, but not
- so lazy as to not write this comment.
-}
master :: Config -> [NodeId] -> Process ()
master c peers = do
  w <- liftIO (getWorld c)
  let pSteps = getSteps (cImageWidth c) (cImageHeight c) (cChunks c)
      ntodo = length pSteps
  ndone <- liftIO $ C.newMVar 0
  nsent <- liftIO $ C.newMVar 0
  todo <- liftIO C.newChan
  done <- liftIO (C.newChan :: IO (C.Chan (Int,[Color])))
  liftIO $ C.writeList2Chan todo (zip [0 :: Int ..] pSteps)
  ready <- liftIO $ MU.replicate (length peers) True
  liftIO $ mapM_ print peers
  -- then we start slaves
  ps <- forM peers $ \nid -> do
          say $ printf "spawing on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)
  mypid <- getSelfPid

  forM_ ps $ \pid -> do
    say $ printf "sending world to %s" (show pid)
    send pid (MsgWorld mypid w)

  waitForAck ps

  sendAll mypid todo ntodo nsent ready ps

  whileM_ (liftIO ((< ntodo) <$> C.readMVar ndone)) $ do
    sendAll mypid todo ntodo nsent ready ps
    MsgImg pid (i,wID,img) <- expect
    liftIO $ MU.write ready i True
    liftIO $ C.writeChan done (wID,img)
    liftIO $ incMVar ndone
    nc <- liftIO $ C.readMVar ndone
    say $ printf "received %d from %s" wID (show pid)
    say $ printf "%d / %d" nc ntodo

  say "rendering complete, writing ppm"

  img <- liftIO $ concatMap snd . sortOn fst <$> readNextN done ntodo
  liftIO $ writePPM "img.ppm" (round (wImgWd w)) (round (wImgHt w)) img

  say "all done"
  terminate

{- blocks until all processes have sent an acknowledge response. Required 
- since occasianaly work will be sent before the world arrives -}
waitForAck :: [ProcessId] -> Process ()
waitForAck [] = return ()
waitForAck ps = do
  m <- expect
  case m of
    MsgAck p -> waitForAck (filter (/= p) ps)
    _  -> say "MASTER received notack" >> terminate

{- sends work to every worker that is not occupied
- we only send a worker work if it is idle and there is work to be done
-}
sendAll :: ProcessId
        -> C.Chan (Int,(Int,Int))
        -> Int
        -> C.MVar Int
        -> MU.IOVector Bool
        -> [ProcessId]
        -> Process ()
sendAll mypid todo ntodo nsent ready ps = do
  numsent0 <- liftIO $ C.readMVar nsent
  say $ printf "ntodo %d nsent %d" ntodo numsent0
  status <- liftIO (U.freeze ready)
  say $ printf "status: %s" (show status)
  forM_ (zip [0..] ps) $ \(i,pid) -> do
    numsent <- liftIO $ C.readMVar nsent
    idle <- liftIO $ MU.read ready i
    when (idle && numsent < ntodo) $ do
      (wID,(start,step)) <- liftIO $ C.readChan todo
      say $ printf "sending %d(%d-%d) to %s" wID start (start + step) (show pid)
      send pid (MsgWork mypid (i,wID,start,step))
      liftIO $ incMVar nsent
      liftIO $ MU.write ready i False

readNextN :: C.Chan a -> Int -> IO [a]
readNextN _ 0 = return []
readNextN ch n = C.readChan ch >>= \x -> (x:) <$> readNextN ch (n - 1)

incMVar :: C.MVar Int -> IO ()
incMVar mv = do
  i <- C.takeMVar mv
  C.putMVar mv (i + 1)

replaceGenomes :: Population -> [Genome] -> Population
replaceGenomes pop gns = map (\(a,b,c,d,gs) -> (a,b,c,d,replace gs gns)) pop
  where
    replace :: [Genome] -> [Genome] -> [Genome]
    replace [] _ = []
    replace g [] = g
    replace (x:xs) gs = fndMatch x gs : replace xs gs
    fndMatch :: Genome -> [Genome] -> Genome
    fndMatch x [] = x
    fndMatch x (g:gs)
      | x `eq` g = g
      | otherwise = fndMatch x gs
    eq (Genome _ x y) (Genome _ x1 y1) = x == x1 && y == y1

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

main :: IO ()
main = distribMain (master bench6Config) Main.__remoteTable
