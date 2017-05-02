module Server
  (runServer
  ) where

import ConcurrentUtils
import Control.Monad
import Network
import System.IO
import Models
import Messages (decode, encode)

runServer :: ServerId -> CurrentConnection -> ConnectionDB -> MessageQueues -> IO ()
runServer selfId _ _ _ = do
  sock <- listenOn defaultPort
  forever $ do
     (handle, host, port) <- accept sock
     forkFinally (talk selfId handle) (\_ -> hClose handle)

talk :: ServerId -> Handle -> IO ()
talk selfId h = do
    hSetBuffering h LineBuffering
    forever $ do
      hGetLine h >>= (handleMessage selfId h)

handleMessage :: ServerId -> Handle -> String -> IO ()
handleMessage selfId h msg =
    case decode msg of
      Just (Handshake id) -> hPutStrLn h (encode $ Handshake selfId)
      Just (Msg id msg) -> putStrLn msg
      Nothing -> return ()
