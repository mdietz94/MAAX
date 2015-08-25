{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
module Main where

import Types
import qualified Mario as M
import NeuralNetwork (loadPopulation, savePopulation, fittestGenome, marioConfig)
import Emulator (saveAsFM2)

import qualified Control.Concurrent as C
import Control.Concurrent.STM.TVar
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Control.Monad.Loops
import Data.List (sortOn)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Text.Printf
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import System.Environment
import System.Directory

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
master c peers = do

  liftIO $ mapM_ print peers

  -- then we start slaves
  ps <- forM peers $ \nid -> do
          say $ printf "spawing on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)
  mypid <- getSelfPid

  mapM_ monitor ps


  (num,p0) <- liftIO loadMaxPopulation
  runPopulation ps p0 num

  say "all done"
  terminate


sender mypid ps ready ntodo nsent ndone todo done =
  whileM_ (liftIO ((< ntodo) <$> C.readMVar ndone)) $ do
    sendAll mypid ps ready ntodo nsent todo
    receiveWait 
      [ match $ \(ProcessMonitorNotification ref pid reason) ->
          say (show pid ++ " died: " ++ show reason) --TODO cleanup this
      , match $ \(MsgG pid g') -> do
          C.writeChan done g'
          liftIO $ incMVar ndone
      ]
    
sendAll mypid ps ready ntodo nsent todo =
  forM_ (zip [0..] ps) $ \(i,pid) -> do
    idle <- liftIO $ MU.read ready i
    numsent <- liftIO $ C.readMVar nsent
    when (idle && numsent < ntodo) $ do
      g <- liftIO $ C.readChan todo
      send pid (MsgG mypid genome)
      liftIO $ incMVar nsent
      liftIO $ MU.write ready i False

recAll [] = return []
recAll (port:ps) =
  receiveWait
    [ match $ \(ProcessMonitorNotification ref pid reason) -> do
        say (show pid ++ " died: " ++ show reason)
        recAll (port:ps)
    , matchChan port $ \p -> do
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


--runPopulation :: (Int, Population, [Float]) -> Int -> Socket -> IO b
runPopulation peers (gInnov,p0,gen) n = do
  let genomes = concatMap (\(_,_,_,_,gs) -> gs) p0
  genomes' <- run peers genomes
  let p1 = replaceGenomes p0 genomes'
  savePopulation ("./data/" ++ show n ++ ".bin") p1
  let (gInnov',p2,gen') = M.stepNetwork p0 (gInnov,p1,gen) marioConfig
  savePopulation ("./data/" ++ show (n+1) ++ ".bin") p2
  joydata <- M.recordMario (fittestGenome p2)
  saveAsFM2 ("./data/" ++ show (n+1) ++ ".fm2") joydata
  runPopulation peers (gInnov',p2,gen') (n+1)

--run :: [Genome] -> Socket -> IO [Genome]
run :: [ProcessId] -> [Genome] -> Process [Genome]
run ps genomes = do
  let ntodo = length genomes
  ndone <- liftIO $ C.newMVar 0
  todo <- liftIO (C.newChan :: IO (C.Chan (Int,Genome)))
  done <- liftIO (C.newChan :: IO (C.Chan (Int,Genome)))
  ready <- liftIO $ MU.replicate (length ps) True
  liftIO $ C.writeList2Chan todo (zip [0..] genomes)
  sender ps ntodo ndone todo done ps
  liftIO $ snd <$> readNextN done ntodo


main :: IO ()
main = distribMain master Main.__remoteTable
