{-# LANGUAGE OverloadedStrings #-}

import Types
import qualified Mario as M

import Data.Binary
import Control.Exception
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Char8 (hPutStrLn)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Control.Concurrent
import Options.Applicative
import System.IO hiding (hPutStrLn)
import System.IO.Error

--if debug then will not run mario, just sets fitness to 42 and sends back
debug :: Bool
debug = False

main :: IO ()
main = execParser opts >>= startMaster
  where
    opts = info (helper <*> optParser)
                (fullDesc <> 
                 progDesc "Evaluates fitness of Neural Networks in Mario" <> 
                 header "MAAX-worker" )


startMaster :: Opts -> IO ()
startMaster opts = withSocketsDo $ loop opts 10 10 1


-- t is how long to wait before retrying in seconds
loop :: Opts -> Int -> Int -> Int -> IO ()
loop opts _ 0 _ = void $ putStrLn $ show opts ++  " not responding . . . stopping"
loop opts nmax n t = catch (loopListen opts) handler
  where
    handler e
      | isDoesNotExistError e = print e >>
                                putStrLn ("trying again in " ++ show t) >>
                                threadDelay (10 ^ (6 :: Int) * t) >>
                                loop opts nmax (n - 1) (t*2)
      | otherwise = print e >> loop opts nmax nmax 1

loopListen :: Opts -> IO ()
loopListen (Opts ip port) = do
    infos <- getAddrInfo Nothing (Just ip) (Just port)
    sock <- socket AF_INET Stream 0
    connect sock (addrAddress . head $ infos)
    sHandle <- socketToHandle sock ReadWriteMode
    hSetBuffering sHandle NoBuffering
    runConn sHandle
    hClose sHandle

runConn :: Handle -> IO ()
runConn sHandle = do
  nbytes <- strictDecode <$> B.hGetLine sHandle
  unless (nbytes < 1)
   $ do g0bs <- B.hGet sHandle nbytes
        putStrLn $ "received " ++ show nbytes
        g0 <- strictDecode <$> return g0bs :: IO Genome
        g1 <- strictEncode <$> if debug then return $ set fitness 42 g0
                                        else M.runMario g0
        let g1bytes = strictEncode $ B.length g1
        hPutStrLn sHandle g1bytes
        B.hPut sHandle g1
        putStrLn $ "sent " ++ show (B.length g1)
        runConn sHandle

strictEncode :: Binary a => a -> B.ByteString
strictEncode = BL.toStrict . encode

strictDecode :: Binary a => B.ByteString -> a
strictDecode = decode . BL.fromStrict

data Opts = Opts String String

instance Show Opts where
  show (Opts ip port) = ip ++ ':' : port

optParser :: Parser Opts
optParser= Opts <$> strOption (short 'm' <>
                               long "master" <>
                               metavar "MASTER_IP" <>
                               help "IP address of server running master")
                <*> strOption (short 'p' <>
                               long "port" <>
                               metavar "MASTER_PORT" <>
                               help "Port master is listening on")
