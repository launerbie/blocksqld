{-# LANGUAGE OverloadedStrings   #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Pool


import Database.Persist.Sql

import Text.Pretty.Simple (pPrint)

import Network.HTTP.Client
import Network.HTTP.Types

import Blocksqld.Types
import Blocksqld.Database
import Blocksqld.Commands

defaultBlocksqldConfig :: IO (DBConfig, CoinConf)
defaultBlocksqldConfig = parseConfig "config.txt"

main :: IO ()
main = do
  (dbcfg, coincfg) <- defaultBlocksqldConfig
  pool <- runReaderT createPoolandMigrate dbcfg
  mgr  <- newManager defaultManagerSettings
  let appConf = AppConf mgr pool dbcfg coincfg
  startServer appConf

startServer :: AppConf -> IO ()
startServer conf = do
  --runReaderT test conf
  runApp testDB conf
  return ()

testDB :: AppM ()
testDB = do
  pool <- lift $ asks appPool
  height <- getblockcount
  let intss = chunks 10000 [600000..height]

  forM_ intss $ \ints -> do
    blocks <- mapM getblockWithHeight ints
    liftIO $ do insertBlocks pool blocks
                print "Inserted block:"
                pPrint $ head blocks

test :: AppReaderT ()
test = do
  appconf <- ask
  lift $ forM_ [1..] $ \i -> do
      pPrint =<< runApp (getblockWithHeight i) appconf

