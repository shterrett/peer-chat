module Main where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import qualified Data.Map.Strict as Map

import Client (runClient)
import Server (runServer)

main :: IO ()
main = do
    conn <- newTVarIO Nothing
    connDB <- newTVarIO (Map.empty)
    (forkIO $ runServer conn connDB) >> runClient conn connDB
