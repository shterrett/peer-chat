module Client
  ( runClient
  , addConnection
  ) where

import Control.Monad
import Control.Monad.STM
import qualified Data.Map.Strict as Map
import Network
import System.IO
import System.Exit
import Control.Concurrent.STM.TVar
import Models
import Commands (Command(Message, Add, Switch, Remove, Quit), command)

runClient :: CurrentConnection -> ConnectionDB -> IO ()
runClient conn db = do
    forever $ getLine >>=
              (handleCommand conn db . command)


handleCommand :: CurrentConnection -> ConnectionDB -> Either String Command -> IO ()
handleCommand _ _ (Left e) = putStrLn e
handleCommand conn connDB (Right c) = handle c
  where handle (Message s) = sendMessage conn s
        handle (Add c) = addConnection connDB c >> connectToServer conn c
        handle (Switch name) = switchConnection connDB conn name
        handle (Remove name) = removeConnection connDB conn name
        handle Quit = exitSuccess

sendMessage :: CurrentConnection -> String -> IO ()
sendMessage conn message = withServer conn $ (flip hPutStrLn) message

addConnection :: ConnectionDB -> Connection -> IO ()
addConnection db new = atomically $ insertConnection db new
  where insertConnection db new = readTVar db >>= updateDB db new
        updateDB db new map = writeTVar db $ Map.insert (name new) new map

switchConnection :: ConnectionDB -> CurrentConnection -> ConnectionName -> IO ()
switchConnection db curr name = (newConn db name) >>= mapM_ (connectToServer curr)
  where newConn db name = atomically (fmap (Map.lookup name) (readTVar db))

removeConnection :: ConnectionDB -> CurrentConnection -> ConnectionName -> IO ()
removeConnection db curr name = atomically $ deleteConnection db name
  where deleteConnection db name = readTVar db >>= updateDB db name
        updateDB db name map = writeTVar db $ Map.delete name map

connectToServer :: CurrentConnection -> Connection -> IO ()
connectToServer conn (Connection { addr = a, port = p }) =
    newServer >>= atomically . writeTVar conn . Just
  where newServer = connectTo a p

withServer :: CurrentConnection -> (Handle -> IO ()) -> IO ()
withServer s action = (atomically $ readTVar s) >>= mapM_ action
