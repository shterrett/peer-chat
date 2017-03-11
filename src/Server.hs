module Server
  (runServer
  ) where

import ConcurrentUtils
import Network
import Control.Monad
import System.IO

port :: Int
port = 44444

runServer :: IO ()
runServer = do
  sock <- listenOn (PortNumber (fromIntegral port))
  forever $ do
     (handle, host, port) <- accept sock
     forkFinally (talk handle) (\_ -> hClose handle)

talk :: Handle -> IO ()
talk h = do
    hSetBuffering h LineBuffering
    forever $ do
      (hGetLine h) >>= (hPutStrLn h)
