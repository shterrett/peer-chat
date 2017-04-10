module Models where

import Network
import Control.Concurrent.STM.TVar
import System.IO
import qualified Data.Map.Strict as Map

type ConnectionName = String
data Connection =
    Connection { name :: ConnectionName
               , addr :: HostName
               , port :: PortID
               }
    deriving(Eq, Show)

type CurrentConnection = TVar (Maybe Handle)

type ConnectionDB = TVar (Map.Map ConnectionName Connection)

defaultPort :: PortID
defaultPort = PortNumber 90909
