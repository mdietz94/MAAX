{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
module Main where

import Types hiding (genome)
import qualified Mario as M
import NeuralNetwork (loadPopulation, savePopulation, fittestGenome, marioConfig)
import Emulator (saveAsFM2)

import Control.Concurrent (threadDelay)
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Lens (over,(^.))
import Control.Monad
import Text.Printf
import Data.Binary
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Typeable
import GHC.Generics (Generic)
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
             | MsgInit ProcessId
             | MsgNoWork ProcessId
             | MsgGenome ProcessId (Int,Genome)
             | MsgFitness ProcessId (Maybe (Int,Float))
             deriving (Typeable,Generic)

instance Binary Message

{- The clients wait for genomes, run the genomes, then send the fitness
- back -}
pingServer :: Process ()
pingServer = do
  MsgInit from <- expect
  mypid <- getSelfPid
  send from (MsgFitness mypid Nothing)
  forever $
    receiveWait 
      [ match $ \(MsgNoWork _) -> do
          liftIO $ threadDelay 1000000
          send from (MsgFitness mypid Nothing)
      , match $ \(MsgGenome _ (gid,genome0)) -> do
          genome <- liftIO $ M.runMario genome0
          send from (MsgFitness mypid (Just (gid,genome^.fitness)))
      ]

remotable ['pingServer]

master :: Config -> [NodeId] -> Process ()
master _ peers = do

  liftIO $ mapM_ print peers

  -- then we start slaves
  ps <- forM peers $ \nid -> do
          say $ printf "spawing on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)
  mypid <- getSelfPid
  forM_ ps $ \pid -> send pid (MsgInit mypid)

  mapM_ monitor ps --will let us know if slave fails

  (num,p0) <- liftIO loadMaxPopulation
  _ <- runPopulation (0,p0) num

  say "all done"
  terminate

runPopulation :: (Int,Population) -> Int -> Process b
runPopulation (gInnov,p0) n = do
  p1 <- run p0
  liftIO $ savePopulation ("./data/" ++ show n ++ ".bin") p1
  rgen <- liftIO newStdGen
  let rfs = randomRs (0.0,1.0) rgen
      (gInnov',p2,_) = M.stepNetwork p0 (gInnov,p1,rfs) marioConfig
  liftIO $ savePopulation ("./data/" ++ show (n+1) ++ ".bin") p2
  joydata <- liftIO $ M.recordMario (fittestGenome p2)
  liftIO $ saveAsFM2 ("./data/" ++ show (n+1) ++ ".fm2") joydata
  runPopulation (gInnov',p2) (n+1)

run :: Population -> Process Population
run pop = do
  let sizes = map (\(_,_,_,_,gs) -> length gs) pop
      genomes0 = concatMap (\(_,_,_,_,gs) -> gs) pop
  genomes' <- sendAll genomes0
  let genomes'' = splitInto sizes genomes'
      pop' = zipWith (\(a,b,c,d,_) gs -> (a,b,c,d,gs)) pop genomes''
  return pop'

sendAll :: [Genome] -> Process [Genome]
sendAll todo0 = do
  mypid <- getSelfPid
  fs <- map snd . sortOn fst <$> loop mypid (zip [0..] todo0) [] Map.empty
  return $ zipWith (over fitness . const) fs todo0
  where
    ntodo = length todo0
    loop mypid todo done info
      | length done == ntodo = return done
      | otherwise =
        receiveWait
          [ match $ \(ProcessMonitorNotification ref pid reason) -> do
              say (show pid ++ " ref: " ++ show ref ++ " died: " ++ show reason)
              case Map.lookup pid info of
                Nothing -> loop mypid todo done info
                (Just w) -> loop mypid (w : todo) done (Map.delete pid info)
          , match $ \(MsgFitness from g) -> do
              let done' = maybe done (:done) g
              if not (null todo)
                then do
                  let (work : todo') = todo
                  send from (MsgGenome mypid work)
                  loop mypid todo' done' (Map.insert from work info)
                else do
                  send from (MsgNoWork mypid)
                  loop mypid todo done' (Map.delete from info)
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

splitInto :: [Int] -> [a] -> [[a]]
splitInto [] as = [as]
splitInto (i:is) as = let (xs,ys) = splitAt i as in xs : splitInto is ys


main :: IO ()
main = distribMain (master marioConfig) Main.__remoteTable
