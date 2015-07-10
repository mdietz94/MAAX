{-# LANGUAGE OverloadedStrings #-}
import Types

import Network.Socket

main = do
  sock <- socket AF_INET Stream 0
  worker <- inet_addr "127.0.0.1"
  connect sock (SockAddrInet 3000 worker)
  send sock "hello"
  sClose sock
