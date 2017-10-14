{-# LANGUAGE OverloadedStrings   #-}
module Main where

import Control.Concurrent
import Control.Exception (try)
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import qualified Data.Configurator as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Base64 as B64
import Database.Persist.Postgresql

import Text.Pretty.Simple (pPrint)

import Network.HTTP.Client
import Network.HTTP.Types

import Blocksqld.Types
import Blocksqld.Schema
import Blocksqld.Commands

type DBHandler = ReaderT DBConfig
type CoinHandler = ReaderT CoinConf

main :: IO ()
main = do
  --runReaderT startDB dbcfg
  forever $ do threadDelay 2000000
               test

startDB :: ReaderT DBConfig IO ()
startDB = do
  dbcfg <- ask
  let conn = getConnString dbcfg
  lift $ do pgpool <- runNoLoggingT $ createPostgresqlPool conn 10
            runSqlPool (runMigration migrateAll) pgpool

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

-- use applyBasicAuth instead
getAuthHeader :: String -> String -> Header
getAuthHeader user pass  =
  let enc = (B64.encode . S8.pack)
      b   = enc (user++":"++pass)
  in ("Authorization", "Basic " `S8.append` b)

fromRpcToRequest ::  RpcRequest -> CoinHandler IO Request
fromRpcToRequest rpc = do
  host <- asks coinHost
  port <- asks coinPort
  u    <- asks coinRpcUser
  p    <- asks coinRpcPass
  let authheader = getAuthHeader u p
  initReq <- parseRequest $ "http://"++host++":"++(show port)
  return $ createJsonRpc initReq rpc authheader

runHTTPRequests :: [Request] -> IO [Response BL.ByteString]
runHTTPRequests reqs = do
    mgr <- newManager defaultManagerSettings
    mapM (flip httpLbs mgr) reqs

test :: IO ()
test = do
  (dbcfg, coincfg) <- parseConfig "config.txt"
  rs <- runReaderT (mapM (fromRpcToRequest . getblockhash) [1..20]) coincfg
  resp <- runHTTPRequests rs
  let resp2 = fmap responseBody resp
  print resp2

