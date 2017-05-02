module Utils where

import Control.Monad
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Models

currentServerId :: CurrentConnection -> ConnectionDB -> IO (Maybe ServerId)
currentServerId conn db =
    currentConnectionName >>=
      (\mn -> join <$> sequence (connectionId <$> mn))
  where
    currentConnectionName = atomically $ (fmap fst) <$> readTVar conn :: IO (Maybe ConnectionName)
    connectionId name = atomically $ (\map -> (Map.lookup name map) >>= serverId) <$> readTVar db :: IO (Maybe ServerId)
