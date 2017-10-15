{-# LANGUAGE OverloadedStrings   #-}
module Blocksqld.Types where

import Control.Monad
import Control.Monad.Trans.Reader

import qualified Data.Configurator as C
import qualified Data.ByteString.Char8 as S8

import Data.Aeson
import Data.Scientific
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

--data Block = Block { hash              :: String
--                  -- , confirmations     :: Int
--                   , size              :: Int
--                   , height            :: Int
--                   , versionBlock      :: Int
--                   , merkleroot        :: String
--                   , txs               :: [String]
--                   , time              :: Int
--                   , nonce             :: Int
--                   , bits              :: String
--                   , difficulty        :: Scientific
--                   , chainwork         :: String
--      --             , previousblockhash :: String
--      --             , nextblockhash     :: Maybe String
--                   } deriving (Show)

--instance FromJSON Block where
--    parseJSON (Object v) = Block <$> v .: "hash"
--    --                             <*> v .: "confirmations"
--                                 <*> v .: "size"
--                                 <*> v .: "height"
--                                 <*> v .: "version"
--                                 <*> v .: "merkleroot"
--                                 <*> v .: "tx"
--                                 <*> v .: "time"
--                                 <*> v .: "nonce"
--                                 <*> v .: "bits"
--                                 <*> v .: "difficulty"
--                                 <*> v .: "chainwork"
--     --                            <*> v .: "previousblockhash"
--     --                            <*> v .:? "nextblockhash"
--    parseJSON _ = mzero
--
