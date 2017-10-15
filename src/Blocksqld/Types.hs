{-# LANGUAGE OverloadedStrings   #-}
module Blocksqld.Types where

import Control.Monad.Trans.Reader

import qualified Data.Configurator as C
import qualified Data.ByteString.Char8 as S8

import Data.Aeson
import Database.Persist.Postgresql
import Network.Socket

type DBHandler = ReaderT DBConfig
type CoinHandler = ReaderT CoinConf


data DBConfig = DBConfig
    { dbHost :: HostName
    , dbPort :: Int
    , dbName :: String
    , dbUser :: String
    , dbPass :: String
    } deriving (Show)

data CoinConf = CoinConf
    { coinName :: String
    , coinHost :: String
    , coinPort :: Int
    , coinRpcUser :: String
    , coinRpcPass :: String
    } deriving (Show)

parseConfig :: FilePath -> IO (DBConfig, CoinConf)
parseConfig f = do
    cfg <- C.load [C.Required f]
    dbcfg   <- DBConfig <$> C.require cfg "postgres.host"
                        <*> C.require cfg "postgres.port"
                        <*> C.require cfg "postgres.db"
                        <*> C.require cfg "postgres.user"
                        <*> C.require cfg "postgres.passwd"

    coincfg <- CoinConf <$> C.require cfg "coin.name"
                        <*> C.require cfg "coin.host"
                        <*> C.require cfg "coin.port"
                        <*> C.require cfg "coin.rpcuser"
                        <*> C.require cfg "coin.rpcpass"
    return (dbcfg, coincfg)

getConnString :: DBConfig -> ConnectionString
getConnString p = S8.pack $ concat [ "host=", (dbHost p)
                                  , " dbname=", (dbName p)
                                  , " user=", (dbUser p)
                                  , " password=", (dbPass p)
                                  , " port=", show (dbPort p)
                                  ]

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
