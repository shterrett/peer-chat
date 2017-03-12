module Client
  (runClient
  ) where

import Control.Monad
import Network
import System.IO

runClient :: IO ()
runClient =
    connectTo "localhost" (PortNumber 44444) >>= (\handle ->
        forever $ getLine >>=
                  (hPutStrLn handle) >>
                  (hGetLine handle) >>=
                  putStrLn
    )
