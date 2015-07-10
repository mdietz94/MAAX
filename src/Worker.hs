
import Types
import qualified Mario as M

import Debug.Trace

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
  cnts <- hGetContents handle
  print cnts
  hClose handle

 
msg = "Pong!\r\n"
