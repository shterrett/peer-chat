module Client
  (runClient
  ) where

import Control.Monad
import Control.Monad.STM
import Network
import System.IO
import Control.Concurrent.STM.TVar
import Commands (Command(Message, Add, Switch, Remove, Quit), command)

type CurrentServer = TVar (Maybe (IO Handle))

runClient :: IO ()
runClient = do
    server <- newTVarIO Nothing
    forever $ getLine >>=
              (handleCommand server . command) >>
              getMessage server


handleCommand :: CurrentServer -> Either String Command -> IO ()
handleCommand server (Left e) = putStrLn $ show e
handleCommand server (Right c) = handle c
  where handle (Message s) = sendMessage server s
        handle (Add c) = putStrLn $ show c
        handle (Switch name) = putStrLn $ show name
        handle (Remove name) = putStrLn $ show name
        handle Quit = putStrLn "quit"

sendMessage :: CurrentServer -> String -> IO ()
sendMessage server message = withServer server $ (flip hPutStrLn) message

getMessage :: CurrentServer -> IO ()
getMessage server = withServer server $ putStrLn <=< hGetLine

withServer :: CurrentServer -> (Handle -> IO ()) -> IO ()
withServer s action = (atomically $ readTVar s) >>=
                      mapM_ (liftM action)
