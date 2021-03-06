{-# LANGUAGE OverloadedStrings   #-}
module Blocksqld.Types where


import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either

import qualified Data.Configurator as C
import qualified Data.ByteString.Char8 as S8

import Data.Aeson
import Data.Scientific
import Data.Pool
import Database.Persist.Postgresql
import Network.Socket
import Network.HTTP.Client
import Network.HTTP.Types

type DBHandler = ReaderT DBConfig
type CoinHandler = ReaderT CoinConf
--type CommandM = EitherT String (ReaderT CoinConf IO)
type AppReaderT = ReaderT AppConf IO
type AppM = EitherT String (AppReaderT)

data AppConf = AppConf
  { manager   :: Manager
  , appPool   :: Pool SqlBackend
  , dbConf    :: DBConfig
  , coinConf  :: CoinConf
  }

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

