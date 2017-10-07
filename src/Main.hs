{-# LANGUAGE OverloadedStrings   #-}
module Main where

import Control.Monad.Logger
import Control.Monad.Except
import Control.Monad.Trans.Except
import qualified Data.Configurator as C
import qualified Data.ByteString.Char8 as B
import Database.Persist.Postgresql

import Blocksqld.Types
import Blocksqld.Schema

main :: IO ()
main = do
  cfg <- parseConfig "config.txt"
  pgpool <- runNoLoggingT $ createPostgresqlPool (getConnString cfg) 10
  runSqlPool (runMigration migrateAll) pgpool
  return ()

parseConfig :: FilePath -> IO AppConfig
parseConfig f = do
    cfg <- C.load [C.Required f]
    host   <- C.require cfg "postgres.host"
    port   <- C.require cfg "postgres.port"
    dbname <- C.require cfg "postgres.db"
    user   <- C.require cfg "postgres.user"
    passwd <- C.require cfg "postgres.passwd"
    return (AppConfig host port dbname user passwd)

getConnString :: AppConfig -> ConnectionString
getConnString p = B.pack $ concat [ "host=", (dbHost p)
                                  , " dbname=", (dbName p)
                                  , " user=", (dbUser p)
                                  , " password=", (dbPass p)
                                  , " port=", show (dbPort p)
                                  ]

