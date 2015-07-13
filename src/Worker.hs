{-# LANGUAGE OverloadedStrings #-}
module Worker where
import Types
import qualified Mario as M

import Debug.Trace

import Data.Binary
import Data.Typeable
import Control.Exception
import Control.Lens
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Char8 (hPutStrLn)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Concurrent
import System.IO hiding (hPutStrLn)
import System.IO.Error

ip = "127.0.0.1"

--if debug then will not run mario, just sets fitness to 42 and sends back
debug = False

main = withSocketsDo $ loop 10 10 1


-- n is number of times to try to connect after ConnectionRefused
-- t is how long to wait before retrying in seconds
loop _ 0 _ = void $ print "Master not responding . . . stopping"
loop nmax n t = catch loopListen handler
  where
    handler e
      | isDoesNotExistError e = print e >>
                                putStrLn ("trying again in " ++ show t) >>
                                threadDelay (10^6 * t) >>
                                loop nmax (n - 1) (t*2)
      | otherwise = print e >> loop nmax nmax 1


loopListen = do
    infos <- getAddrInfo Nothing (Just ip) (Just "3000")
    sock <- socket AF_INET Stream 0
    connect sock (addrAddress . head $ infos)
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering
    runConn handle
    hClose handle

runConn handle = do
  numbytes <- B.hGetLine handle
  let nbytes = strictDecode numbytes
  unless (nbytes < 1)
   $ do str <- B.hGet handle nbytes
        putStrLn $ "received " ++ show nbytes
        g0 <- strictDecode <$> return str :: IO Genome
        g1 <- strictEncode <$> if debug then return $ set fitness 42 g0
                                        else M.runMario g0
        let g1bytes = strictEncode $ B.length g1
        hPutStrLn handle g1bytes
        B.hPut handle g1
        putStrLn $ "sent " ++ show (B.length g1)
        runConn handle

strictEncode :: Binary a => a -> B.ByteString
strictEncode = BL.toStrict . encode

strictDecode :: Binary a => B.ByteString -> a
strictDecode = decode . BL.fromStrict
