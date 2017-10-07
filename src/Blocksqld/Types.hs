{-# LANGUAGE OverloadedStrings   #-}
module Blocksqld.Types where

import Data.Aeson
import Network.Socket

data AppConfig = AppConfig
    { dbHost :: HostName
    , dbPort :: Int
    , dbName :: String
    , dbUser :: String
    , dbPass :: String
    } deriving (Show)

data RpcRequest = RpcRequest { rpcMethod :: Value
                             , rpcParams :: [Value]
                             , rpcId :: Value
                             } deriving (Show)

data RpcResponse = RpcResponse { rpcResult :: Value
                               , rpcError :: Value
                               , rpcResponseId :: Value
                               } deriving (Show)

instance ToJSON RpcRequest where
    toJSON (RpcRequest method params id) =
        object ["method" .= method , "params" .= params , "id" .= id]

instance FromJSON RpcResponse where
  parseJSON (Object v) = RpcResponse <$> v .: "result"
                                     <*> v .: "error"
                                     <*> v .: "id"
