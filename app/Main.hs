module Main where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import qualified Data.Map.Strict as Map
import qualified Data.UUID.V4 as UUID

import Client (runClient)
import Server (runServer)

main :: IO ()
main = do
    conn <- newTVarIO Nothing
    connDB <- newTVarIO (Map.empty)
    selfId <- UUID.nextRandom
    (forkIO $ runServer selfId conn connDB) >> runClient selfId conn connDB
