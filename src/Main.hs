{-# LANGUAGE OverloadedStrings   #-}
module Main where

import Control.Monad.Logger
import Control.Monad.Except
import Control.Monad.Trans.Except
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
  (dbcfg, coincfg) <- parseConfig "config.txt"
  pPrint dbcfg
  pPrint coincfg
  pgpool <- runNoLoggingT $ createPostgresqlPool (getConnString dbcfg) 10
  runSqlPool (runMigration migrateAll) pgpool

  return ()

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

rpc :: RpcRequest -> CoinHandler IO (Response BL.ByteString)
rpc r = do
  cfg  <- ask
  host <- asks coinHost
  port <- asks coinPort
  u    <- asks coinRpcUser
  p    <- asks coinRpcPass
  let authheader = getAuthHeader u p
  liftIO $ do
    initReq <- parseRequest $ "http://"++host++":"++(show port)
    manager <- newManager defaultManagerSettings
    httpLbs (addHeader (setRPCRequest initReq r) authheader) manager


