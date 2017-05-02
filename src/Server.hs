module Server
  (runServer
  ) where

import qualified Data.Map.Strict as Map
import ConcurrentUtils
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Network
import System.IO
import Models
import Messages (decode, encode)

runServer :: ServerId -> CurrentConnection -> ConnectionDB -> MessageQueues -> IO ()
runServer selfId conn db qs = do
  sock <- listenOn defaultPort
  forever $ do
     (handle, host, port) <- accept sock
     forkFinally (talk selfId handle conn db qs) (\_ -> hClose handle)

talk :: ServerId -> Handle -> CurrentConnection -> ConnectionDB -> MessageQueues -> IO ()
talk selfId h conn db qs = do
    hSetBuffering h LineBuffering
    forever $ do
      hGetLine h >>= (handleMessage selfId h conn db qs)

handleMessage :: ServerId -> Handle -> CurrentConnection -> ConnectionDB -> MessageQueues -> String -> IO ()
handleMessage selfId h conn db qs msg =
    case decode msg of
      Just (Handshake id) -> hPutStrLn h (encode $ Handshake selfId)
      Just (Msg id msg) -> printOrQueue conn db qs id msg
      Nothing -> return ()

printOrQueue :: CurrentConnection -> ConnectionDB -> MessageQueues -> ServerId -> String -> IO ()
printOrQueue conn db qs id msg = do
    currId <- currentServerId conn db
    case currId of
      Just cid -> if cid == id
                  then putStrLn msg
                  else return ()
      Nothing -> return ()

currentServerId :: CurrentConnection -> ConnectionDB -> IO (Maybe ServerId)
currentServerId conn db =
    currentConnectionName >>=
      (\mn -> join <$> sequence (connectionId <$> mn))
  where
    currentConnectionName = atomically $ (fmap fst) <$> readTVar conn :: IO (Maybe ConnectionName)
    connectionId name = atomically $ (\map -> (Map.lookup name map) >>= serverId) <$> readTVar db :: IO (Maybe ServerId)
