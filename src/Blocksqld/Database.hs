{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric #-}

module Blocksqld.Database where

import GHC.Generics           (Generic)

import Data.Pool
import Data.Aeson
import Data.ByteString.Char8 as S8 hiding (concat)
import Control.Monad
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import Database.Persist.TH
import Database.Persist.Postgresql
import Database.Persist.Sql

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
    -- Genesis block has no previous block.
    deriving Show
  Tx
    txid     T.Text
    size     Int
    version  Int
    locktime Int
    --vin      T.Text
    --vout     T.Text
    deriving Show
  Transaction 
    blockId     BlockId
    index       Int
    txid        T.Text
    size        Int
    inputCount  Int
    outputCount Int
    inputsBtc   Double
    outputsBtc  Double
    feeBtc      Double
    version     Int
    locktime    Int
    deriving Show
  TransactionInput
    transactionId       TransactionId
    index               Int
    size                Int
    scriptId            Int
    outpointTxid        Int
    outpointInd         Double
    transactionOutputId TransactionOutputId
    value               Int
    sequence            Int
    deriving Show
  TransactionOutput
    transactionId       TransactionId
    index               Int
    toAddressType       Int
    toAddress           T.Text
    value               Int
--    scriptId            ScriptId
--    deriving Show
--  Script
--    deriving Show
  BlockHeader
    hash       T.Text
    size       Int
    height     Int
    version    Int
    merkleroot T.Text
    time       Int
    nonce      Int
    bits       T.Text
    difficulty Double
    chainwork  T.Text
    UniqueHash hash
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

instance FromJSON BlockHeader where
    parseJSON (Object v) =
      BlockHeader <$> v .: "hash"
                  <*> v .: "size"
                  <*> v .: "height"
                  <*> v .: "version"
                  <*> v .: "merkleroot"
                  <*> v .: "time"
                  <*> v .: "nonce"
                  <*> v .: "bits"
                  <*> v .: "difficulty"
                  <*> v .: "chainwork"
    parseJSON _ = mzero

instance FromJSON Tx where
    parseJSON (Object v) = Tx <$> v .: "txid"
                              <*> v .: "size"
                              <*> v .: "version"
                              <*> v .: "locktime"
  --                            <*> v .: "vin"
  --                            <*> v .: "vout"
    parseJSON _ = mzero

---------------------------  ScriptSig ---------------------------------
data ScriptSig = ScriptSig { asmSig :: String
                           , hexSig :: String
                           } deriving (Show)

instance FromJSON ScriptSig where
    parseJSON (Object v) = ScriptSig <$> v .: "asm"
                                     <*> v .: "hex"

---------------------------  ScriptPubKey -------------------------------
data ScriptPubKey = ScriptPubKey { asmPub    :: String
                                 , hexPub    :: String
                                 , reqSigs   :: Int
                                 , type_     :: String
                                 , addresses :: [String]
                                 } deriving (Show)

instance FromJSON ScriptPubKey where
    parseJSON (Object v) = ScriptPubKey <$> v .: "asm"
                                        <*> v .: "hex"
                                        <*> v .: "reqSigs"
                                        <*> v .: "type"
                                        <*> v .: "addresses"
---------------------------  Input -----------------------------------------
data Input = Input { inTx         :: Maybe String
                   , coinbase     :: Maybe String
                   , inVout       :: Maybe Int
                   , inScriptSig  :: Maybe ScriptSig
                   , inSequence   :: Int
                   } deriving (Show, Generic)

--instance ToJSON Input
instance FromJSON Input where
    parseJSON (Object v) = Input <$> v .:? "txid"
                                 <*> v .:? "coinbase"
                                 <*> v .:? "vout"
                                 <*> v .:? "scriptSig"
                                 <*> v .: "sequence"

------------------------  Output  ------------------------------------------
data Output = Output { value        :: Double
                     , n            :: Int
                     , scriptPubKey :: ScriptPubKey
                     } deriving (Show, Generic)

--instance ToJSON Output
instance FromJSON Output
----------------------------------------------------------------------------

connString :: DBHandler IO (S8.ByteString)
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

createPoolandMigrate :: DBHandler IO (Pool SqlBackend)
createPoolandMigrate = do
  c <- connString
  lift $ do pgpool <- runNoLoggingT $ createPostgresqlPool c 10
            runSqlPool (runMigration migrateAll) pgpool
            return pgpool

insertTx ::  Tx -> AppM (Key Tx)
insertTx tx = runDB (insert tx)

insertTxs :: [Tx] -> AppM [Key Tx]
insertTxs txs = runDB $ (mapM insert txs)

insertBlock :: Block -> AppM (Key Block)
insertBlock b = runDB (insert b)

insertBlocks :: [Block] -> AppM [Key Block]
insertBlocks bs = runDB $ (mapM insert bs)

highestBlock :: AppM Int
highestBlock = do
  -- ents :: [Entity Block]
  ents <- runDB $ selectList [] [Desc BlockHeight, LimitTo 1]
  case ents of
    []     -> return (-1)
    (x:xs) -> return (blockHeight $ entityVal x)

runDB :: SqlPersistM a -> AppM a
runDB q = do
  pool     <- lift $ asks appPool
  liftIO $ runSqlPersistMPool q pool

