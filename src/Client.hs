module Client
  (runClient
  ) where

import Control.Monad

runClient :: IO ()
runClient = forever $ do
  getLine >>= putStrLn
