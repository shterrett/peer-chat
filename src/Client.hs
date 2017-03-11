module Client
  (runClient
  ) where

import Control.Monad (forever)

runClient :: IO ()
runClient = forever $ do
  getLine >>= putStrLn
