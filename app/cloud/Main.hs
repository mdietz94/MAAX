{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
module Main where

import Types hiding (genome)
import qualified Mario as M
import NeuralNetwork (loadPopulation, savePopulation, fittestGenome, marioConfig)
import Emulator (saveAsFM2)

import qualified Control.Concurrent as C
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Control.Monad.Loops
import Data.List (sortOn)
--import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Text.Printf
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import System.Environment
import System.Directory
import System.Random

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
    _ -> print "Usage: MAAX-cloud <master|slave> [ <ip_address> <port> ]"


data Message = MsgServers [ProcessId]
             | MsgIdle
             | MsgG ProcessId (Int,Genome)
             | MsgBroadcast Message
             deriving (Typeable,Generic)

instance Binary Message



{- The clients wait for genomes, run the genomes, then send them back -}
pingServer :: Process ()
pingServer =
  forever $ do
    MsgG from (gid,genome) <- expect
    mypid <- getSelfPid
    genome' <- liftIO $ M.runMario genome
    send from (MsgG mypid (gid,genome'))

remotable ['pingServer]

{- The use of MVars in this function is NOT thread safe. The only reason they're
- is because legacy and I'm too lazy to change them to a mutable value, but not
- so lazy as to not write this comment.
-}
master :: Config -> [NodeId] -> Process ()
master _ peers = do

  liftIO $ mapM_ print peers

  -- then we start slaves
  ps <- forM peers $ \nid -> do
          say $ printf "spawing on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)
  mypid <- getSelfPid

  mapM_ monitor ps

  (num,p0) <- liftIO loadMaxPopulation
  let rs = randomRs (0.0,1.0) (mkStdGen 42)
  _ <- runPopulation mypid ps (0,p0,rs) num

  say "all done"
  terminate

sender :: ProcessId
       -> [ProcessId]
       -> MU.IOVector Bool
       -> Int
       -> C.MVar Int
       -> C.MVar Int
       -> C.Chan (Int,Genome)
       -> C.Chan (Int,Genome)
       -> Process ()
sender mypid ps ready ntodo nsent ndone todo done =
  whileM_ (liftIO ((< ntodo) <$> C.readMVar ndone)) $ do
    sendAll mypid ps ready ntodo nsent todo
    receiveWait 
      [ match $ \(ProcessMonitorNotification ref pid reason) ->
          say (show pid ++ " died: " ++ show reason ++ " ref: " ++ show ref) --TODO cleanup this
      , match $ \(MsgG _ g') -> do
          liftIO $ C.writeChan done g'
          liftIO $ incMVar ndone
      ]
   
sendAll :: ProcessId -> [ProcessId] -> MU.IOVector Bool -> Int -> C.MVar Int -> C.Chan (Int,Genome) -> Process ()
sendAll mypid ps ready ntodo nsent todo =
  forM_ (zip [0..] ps) $ \(i,pid) -> do
    idle <- liftIO $ MU.read ready i
    numsent <- liftIO $ C.readMVar nsent
    when (idle && numsent < ntodo) $ do
      g <- liftIO $ C.readChan todo
      send pid (MsgG mypid g)
      liftIO $ incMVar nsent
      liftIO $ MU.write ready i False

recAll :: [ReceivePort a] -> Process [t]
recAll [] = return []
recAll (port:ps) =
  receiveWait
    [ match $ \(ProcessMonitorNotification ref pid reason) -> do
        say (show pid ++ " died: " ++ show reason ++ "\tref: " ++ show ref)
        recAll (port:ps)
    , matchChan port $ \_ -> do
       say "pong on channel"
       recAll ps
    ]



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

readNextN :: C.Chan a -> Int -> IO [a]
readNextN _ 0 = return []
readNextN ch n = C.readChan ch >>= \x -> (x:) <$> readNextN ch (n - 1)

incMVar :: C.MVar Int -> IO ()
incMVar mv = do
  i <- C.takeMVar mv
  C.putMVar mv (i + 1)

runPopulation :: ProcessId -> [ProcessId] -> (Int,Population,[Float]) -> Int -> Process b
runPopulation mypid ps (gInnov,p0,gen) n = do
  p1 <- run mypid ps p0
  liftIO $ savePopulation ("./data/" ++ show n ++ ".bin") p1
  let (gInnov',p2,gen') = M.stepNetwork p0 (gInnov,p1,gen) marioConfig
  liftIO $ savePopulation ("./data/" ++ show (n+1) ++ ".bin") p2
  joydata <- liftIO $ M.recordMario (fittestGenome p2)
  liftIO $ saveAsFM2 ("./data/" ++ show (n+1) ++ ".fm2") joydata
  runPopulation mypid ps (gInnov',p2,gen') (n+1)

--run :: [Genome] -> Socket -> IO [Genome]
run :: ProcessId -> [ProcessId] -> Population -> Process Population
run mypid ps pop = do
  let sizes = map (\(_,_,_,_,gs) -> length gs) pop
      genomes = concatMap (\(_,_,_,_,gs) -> gs) pop
      ntodo = length genomes
  ndone <- liftIO $ C.newMVar 0
  nsent <- liftIO $ C.newMVar 0
  todo <- liftIO (C.newChan :: IO (C.Chan (Int,Genome)))
  done <- liftIO (C.newChan :: IO (C.Chan (Int,Genome)))
  ready <- liftIO $ MU.replicate (length ps) True
  liftIO $ C.writeList2Chan todo (zip [0..] genomes)
  sender mypid ps ready ntodo nsent ndone todo done
  genomes' <- liftIO $ map snd . sortOn fst <$> readNextN done ntodo
  let genomes'' = splitInto sizes genomes'
      pop' = zipWith (\(a,b,c,d,_) gs -> (a,b,c,d,gs)) pop genomes''
  return pop'

splitInto :: [Int] -> [a] -> [[a]]
splitInto [] as = [as]
splitInto (i:is) as = let (xs,ys) = splitAt i as in xs : splitInto is ys


main :: IO ()
main = distribMain (master marioConfig) Main.__remoteTable
