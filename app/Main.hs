module Main where

import Control.Concurrent
import Control.Concurrent.STM.TVar

import Client (runClient)
import Server (runServer)

main :: IO ()
main = do
    conn <- newTVarIO Nothing
    (forkIO $ runServer conn) >> runClient conn
