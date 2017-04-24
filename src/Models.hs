module Models where

import Network
import Control.Concurrent.STM.TVar
import System.IO
import qualified Data.Map.Strict as Map
import Data.UUID

type ConnectionName = String
type ServerId = UUID
data Connection =
    Connection { name :: ConnectionName
               , addr :: HostName
               , port :: PortID
               , serverId :: Maybe ServerId
               }
    deriving(Eq, Show)

type CurrentConnection = TVar (Maybe (ConnectionName, Handle))

type ConnectionMap = Map.Map ConnectionName Connection

type ConnectionDB = TVar ConnectionMap

data Payload =
    Handshake ServerId
    | Msg ServerId String
    deriving (Show, Eq)

defaultPort :: PortID
defaultPort = PortNumber 90909
