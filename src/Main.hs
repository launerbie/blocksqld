{-# LANGUAGE OverloadedStrings   #-}
module Main where

import Control.Monad.Logger
import Control.Monad.Except
import Control.Monad.Trans.Except
import qualified Data.Configurator as C
import qualified Data.ByteString.Char8 as B
import Database.Persist.Postgresql

import Text.Pretty.Simple (pPrint)


import Blocksqld.Types
import Blocksqld.Schema
import Blocksqld.Commands

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
                        <*> C.require cfg "coin.rpcuser"
                        <*> C.require cfg "coin.rpcpass"
    return (dbcfg, coincfg)

getConnString :: DBConfig -> ConnectionString
getConnString p = B.pack $ concat [ "host=", (dbHost p)
                                  , " dbname=", (dbName p)
                                  , " user=", (dbUser p)
                                  , " password=", (dbPass p)
                                  , " port=", show (dbPort p)
                                  ]
