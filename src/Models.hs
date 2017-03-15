module Models where

import Network

type ConnectionName = String
data Connection =
    Connection { name :: ConnectionName
               , addr :: HostName
               , port :: PortID
               }
    deriving(Eq, Show)

defaultPort :: PortID
defaultPort = PortNumber 90909
