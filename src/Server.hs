module Server
  (runServer
  ) where

import ConcurrentUtils
import Control.Monad
import Network
import System.IO
import Models

runServer :: ServerId -> CurrentConnection -> ConnectionDB -> IO ()
runServer _ _ _ = do
  sock <- listenOn defaultPort
  forever $ do
     (handle, host, port) <- accept sock
     forkFinally (talk handle) (\_ -> hClose handle)

talk :: Handle -> IO ()
talk h = do
    hSetBuffering h LineBuffering
    forever $ do
      hGetLine h >>= putStrLn
