module Main where

import Control.Concurrent

import Client (runClient)
import Server (runServer)

main :: IO ()
main = (forkIO runServer) >> runClient
