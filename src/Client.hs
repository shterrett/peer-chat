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
import Messages (encode, decode)
import Utils

runClient :: ServerId -> CurrentConnection -> ConnectionDB -> MessageQueues -> IO ()
runClient selfId conn db qs = do
    forever $ getLine >>=
              (handleCommand selfId conn db qs . command)

handleCommand :: ServerId -> CurrentConnection -> ConnectionDB -> MessageQueues -> Either String Command -> IO ()
handleCommand _ _ _ _ (Left e) = putStrLn e
handleCommand selfId conn connDB qs (Right c) = handle c
  where handle (Message s) = void $ sendMessage conn (Msg selfId s)
        handle (Add c) = addConnection connDB c
                         >> connectToServer conn c
                         >> handshake selfId conn connDB (name c)
                         >> dequeueMessages conn connDB qs
        handle (Switch name) = switchConnection connDB conn name
                               >> handshake selfId conn connDB name
                               >> dequeueMessages conn connDB qs
        handle (Remove name) = removeConnection connDB name
        handle Quit = exitSuccess

sendMessage :: CurrentConnection -> Payload -> IO (Maybe ())
sendMessage conn payload = withServer conn $ (flip hPutStrLn) (encode payload)

addConnection :: ConnectionDB -> Connection -> IO ()
addConnection db new = updateConnectionDB db (Map.insert (name new) new)

removeConnection :: ConnectionDB -> ConnectionName -> IO ()
removeConnection db name = updateConnectionDB db (Map.delete name)

switchConnection :: ConnectionDB -> CurrentConnection -> ConnectionName -> IO ()
switchConnection db curr name = (newConn db name) >>= mapM_ (connectToServer curr)
  where newConn db name = atomically (Map.lookup name <$> readTVar db)

connectToServer :: CurrentConnection -> Connection -> IO ()
connectToServer conn (Connection { addr = a, port = p, name = n }) =
    newServer >>= atomically . writeTVar conn . Just . ((,) n)
  where newServer = connectTo a p

handshake :: ServerId -> CurrentConnection -> ConnectionDB -> ConnectionName ->  IO ()
handshake id conn db name =
    let
      payload = liftM ((=<<) decode)
                      (sendMessage conn (Handshake id)
                        >> withServer conn hGetLine) :: IO (Maybe Payload)
    in
      void $ payload >>= sequence . (fmap updateId)
    where
      updateId (Handshake id) = updateConnectionDB db (setId name id)
      updateId (Msg id _) = updateConnectionDB db (setId name id)
      setId n i d = Map.update (\r -> Just $ r { serverId = (Just i) }) n d

withServer :: CurrentConnection -> (Handle -> IO a) -> IO (Maybe a)
withServer s action =
    let
      handle = atomically $ fmap (fmap snd) (readTVar s) :: IO (Maybe Handle)
    in
      join $ sequence <$> (liftM . fmap) action handle

updateConnectionDB :: ConnectionDB -> (ConnectionMap -> ConnectionMap) -> IO ()
updateConnectionDB db f = atomically $ f <$> readTVar db >>= writeTVar db

dequeueMessages :: CurrentConnection -> ConnectionDB -> MessageQueues -> IO ()
dequeueMessages conn db qs = do
    currId <- currentServerId conn db
    case currId of
      Just id -> printExisting id qs >> clearExisting id qs
      Nothing -> return ()

printExisting :: ServerId -> MessageQueues -> IO ()
printExisting id qs = (atomically $ (Map.findWithDefault [] id) <$> readTVar qs) >>= (putStrLn . unlines)

clearExisting :: ServerId -> MessageQueues -> IO ()
clearExisting id qs = atomically $ (Map.delete id) <$> readTVar qs >>= writeTVar qs
