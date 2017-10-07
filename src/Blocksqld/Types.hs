module Blocksqld.Types where

import Network.Socket

data AppConfig = AppConfig
    { dbHost :: HostName
    , dbPort :: Int
    , dbName :: String
    , dbUser :: String
    , dbPass :: String
    } deriving (Show)


