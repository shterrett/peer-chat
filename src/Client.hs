module Client
  (runClient
  ) where

import Control.Monad
import Control.Monad.STM
import Network
import System.IO
import Control.Concurrent.STM.TVar
import Models
import Commands (Command(Message, Add, Switch, Remove, Quit), command)

runClient :: CurrentConnection -> IO ()
runClient conn = do
    forever $ getLine >>=
              (handleCommand conn . command)


handleCommand :: CurrentConnection -> Either String Command -> IO ()
handleCommand conn (Left e) = putStrLn e
handleCommand conn (Right c) = handle c
  where handle (Message s) = sendMessage conn s
        handle (Add c) = connectToServer conn c
        handle (Switch name) = putStrLn name
        handle (Remove name) = putStrLn name
        handle Quit = putStrLn "quit"

sendMessage :: CurrentConnection -> String -> IO ()
sendMessage conn message = withServer conn $ (flip hPutStrLn) message

getMessage :: CurrentConnection -> IO ()
getMessage conn = withServer conn $ putStrLn <=< hGetLine

connectToServer :: CurrentConnection -> Connection -> IO ()
connectToServer conn (Connection { addr = a, port = p }) =
    newServer >>= atomically . writeTVar conn . Just
  where newServer = connectTo a p

withServer :: CurrentConnection -> (Handle -> IO ()) -> IO ()
withServer s action = (atomically $ readTVar s) >>= mapM_ action
