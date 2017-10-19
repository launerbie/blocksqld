{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Blocksqld.Database where

import Data.Pool
import Data.Aeson
import Data.Monoid
import Data.ByteString.Char8 as S8 hiding (concat)
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import Database.Persist.TH
import Database.Persist.Postgresql

import Data.Time (UTCTime, getCurrentTime, addUTCTime)

import Blocksqld.Types

----------------------- Database Schema ---------------------------------
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Block
    hash       T.Text
    size       Int
    height     Int
    version    Int
    merkleroot T.Text
    tx         [String]
    time       Int
    nonce      Int
    bits       T.Text
    difficulty Double
    chainwork  T.Text
    --previousblockhash T.Text Maybe
    deriving Show
  Tx
    txid     T.Text
    size     Int
    version  Int
    locktime Int
    vin      T.Text
    vout     T.Text
    deriving Show
  BlockTest
    hash       T.Text
    size       Int
    txs        [String]
    deriving Show
|]

instance FromJSON Block where
    parseJSON (Object v) = Block <$> v .: "hash"
    --                             <*> v .: "confirmations"
                                 <*> v .: "size"
                                 <*> v .: "height"
                                 <*> v .: "version"
                                 <*> v .: "merkleroot"
                                 <*> v .: "tx"
                                 <*> v .: "time"
                                 <*> v .: "nonce"
                                 <*> v .: "bits"
                                 <*> v .: "difficulty"
                                 <*> v .: "chainwork"
     --                            <*> v .: "previousblockhash"
     --                            <*> v .:? "nextblockhash"
    parseJSON _ = mzero

instance FromJSON Tx where
    parseJSON (Object v) = Tx <$> v .: "txid"
                              <*> v .: "size"
                              <*> v .: "version"
                              <*> v .: "locktime"
                              <*> v .: "vin"
                              <*> v .: "vout"
    parseJSON _ = mzero

createPoolandMigrate :: ReaderT DBConfig IO (Pool SqlBackend)
createPoolandMigrate = do
  c <- connString
  lift $ do pgpool <- runNoLoggingT $ createPostgresqlPool c 10
            runSqlPool (runMigration migrateAll) pgpool
            return pgpool

insertBlock :: Pool SqlBackend -> Block -> IO (Key Block)
insertBlock p b = runSqlPersistMPool (insert b) p

connString :: ReaderT DBConfig IO (S8.ByteString)
connString = do
  h <- asks dbHost
  n <- asks dbName
  u <- asks dbUser
  p <- asks dbPass
  port <- asks dbPort
  let bs = S8.pack $ concat [ "host=", h
                            , " dbname=", n
                            , " user=",  u
                            , " password=" , p
                            , " port=", show port
                            ]
  return bs
