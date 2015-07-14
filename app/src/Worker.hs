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
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple

--if debug then will not run mario, just sets fitness to 42 and sends back
debug :: Bool
debug = False

main :: IO ()
main = execParser opts >>= startWorker
  where
    opts = info (helper <*> optsParser)
                (fullDesc <> 
                 progDesc "Evaluates fitness of Neural Networks in Mario" <> 
                 header "MAAX-worker" )


startWorker :: Opts -> IO ()
startWorker opts = withSocketsDo $ do
  logH <- fileHandler (_logF opts) INFO >>= \lh -> return $
          setFormatter lh (simpleLogFormatter "[$time : $prio] $msg")
  updateGlobalLogger rootLoggerName (setLevel INFO . addHandler logH)
  infoM rootLoggerName "Worker starting up"
  loop opts 10 10 1


-- t is how long to wait before retrying in seconds
loop :: Opts -> Int -> Int -> Int -> IO ()
loop opts _ 0 _ = errorM rootLoggerName $ show opts ++ " not responding . . . stopping"
loop opts nmax n t = catch (loopListen opts) handler
  where
    handler e
      | isDoesNotExistError e = warningM rootLoggerName (show e ++ " trying again in " ++ show t) >>
                                threadDelay (10 ^ (6 :: Int) * t) >>
                                loop opts nmax (n - 1) (t*2)
      | otherwise = noticeM rootLoggerName (show e) >> 
                    loop opts nmax nmax 1

loopListen :: Opts -> IO ()
loopListen (Opts ip port _) = do
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
        infoM rootLoggerName $ "received " ++ show nbytes ++ " bytes"
        g0 <- strictDecode <$> return g0bs :: IO Genome
        g1 <- strictEncode <$> if debug then return $ set fitness 42 g0
                                        else M.runMario g0
        let g1bytes = strictEncode $ B.length g1
        hPutStrLn sHandle g1bytes
        B.hPut sHandle g1
        infoM rootLoggerName $ "sent " ++ show (B.length g1) ++ " bytes"
        runConn sHandle 

strictEncode :: Binary a => a -> B.ByteString
strictEncode = BL.toStrict . encode

strictDecode :: Binary a => B.ByteString -> a
strictDecode = decode . BL.fromStrict

data Opts = Opts { _ip :: String
                 , _port :: String 
                 , _logF :: String }

instance Show Opts where
  show (Opts ip port _) = ip ++ ':' : port

optsParser :: Parser Opts
optsParser = 
    Opts <$> strOption (short 'm' <>
                       long "master" <>
                       metavar "MASTER_IP" <>
                       value "joomah.com" <>
                       help "IP of master, default joomah.com")
         <*> strOption (short 'p' <>
                       long "port" <>
                       metavar "MASTER_PORT" <>
                       value "3000" <>
                       help "Port master is listening on, default 3000")
         <*> strOption (short 'l' <>
                       long "log" <>
                       metavar "FILE" <>
                       value "worker.log" <>
                       help "File to log messages to, default worker.log")
