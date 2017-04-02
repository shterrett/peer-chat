module Models where

import Network
import Control.Concurrent.STM.TVar
import System.IO

type ConnectionName = String
data Connection =
    Connection { name :: ConnectionName
               , addr :: HostName
               , port :: PortID
               }
    deriving(Eq, Show)

type CurrentConnection = TVar (Maybe Handle)

defaultPort :: PortID
defaultPort = PortNumber 90909
