module Server
  ( start
  ) where

import           Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import           Control.Monad (forever, void)
import           Network.Socket hiding (recv)

start :: ServiceName -> (Socket -> IO ()) -> IO ()
start serviceName handlerFn = do
  addr <- resolve serviceName
  E.bracket (open addr) close (loop handlerFn)

resolve :: ServiceName -> IO AddrInfo
resolve port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE]
                           , addrSocketType = Stream
                           }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  setCloseOnExecIfNeeded (fdSocket sock)
  bind sock (addrAddress addr)
  listen sock 10
  putStrLn $ "Listening on " <> show (addrAddress addr)
  pure sock

loop :: (Socket -> IO ()) -> Socket -> IO ()
loop handlerFn sock = forever $ do
  (conn, peer) <- accept sock
  putStrLn $ "Connection from " ++ show peer
  void $ forkFinally (handlerFn conn) (\_ -> close conn)
