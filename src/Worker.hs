
import Types
import qualified Mario as M

import Debug.Trace

import Data.Binary
import qualified Data.ByteString.Lazy as B
import Network.Socket
import Control.Concurrent
import System.IO


main = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 3000 iNADDR_ANY)
    listen sock 2
    loop sock


loop :: Socket -> IO ()
loop sock = do
  conn <- accept sock
  forkIO (runConn conn)
  loop sock


runConn :: (Socket, SockAddr) -> IO ()
runConn (sock,_) = do
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  g0 <- decode <$> B.hGetContents handle :: IO Genome
  g1 <- M.runMario g0
  B.hPut handle (encode g1)
  hClose handle

 
msg = "Pong!\r\n"
