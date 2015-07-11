{-# LANGUAGE OverloadedStrings #-}
import Types
import qualified Mario as M

import Debug.Trace

import Data.Binary
import Control.Lens 
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Concurrent
import System.IO


--TODO exception and error safe
--TODO B.hPutStrLn deprecated?
main = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 3000 iNADDR_ANY)
    listen sock 2
    loop sock


loop :: Socket -> IO ()
loop sock = do
  conn <- accept sock
  print $ "Connection accepted: " ++ show conn
  runConn conn
  loop sock

--deal with EOF
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock,_) = do
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  loopH handle
  hClose handle
  where loopH handle = do
          numbytes <- B.hGetLine handle
          let nbytes = strictDecode numbytes
          print $ "receiving " ++ show nbytes
          str <- B.hGet handle nbytes
          g0 <- strictDecode <$> return str :: IO Genome
          g1 <- strictEncode <$> M.runMario g0
          let g1bytes = strictEncode $ B.length g1
          print $ "sending " ++ show (B.length g1)
          B.hPutStrLn handle g1bytes
          B.hPut handle g1
          loopH handle
    

strictEncode :: Binary a => a -> B.ByteString
strictEncode = BL.toStrict . encode
strictDecode :: Binary a => B.ByteString -> a
strictDecode = decode . BL.fromStrict
