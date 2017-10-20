{-# LANGUAGE OverloadedStrings   #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
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
  return ()

syncDB :: Pool SqlBackend -> CoinConf -> IO ()
syncDB pool c = do
  eBlocks <- runCommand (mapM getblockWithHeight [1..500]) c
  case eBlocks of
    Left e -> do print "No blocks" >> print e
    Right blocks -> do
       forM_ blocks $ \b -> do
         print "Inserted block:"
         pPrint b
         insertBlock pool b

test :: IO ()
test = do
  (dbcfg, coincfg) <- defaultBlocksqldConfig
  forM_ [1..] $ \i -> do
      threadDelay 2000000
      pPrint =<< runCommand (getblockWithHeight i) coincfg

-- | there is also Data.List.Split.splitEvery
chunks :: (Num a) => Int -> [a] -> [[a]]
chunks s [] = []
chunks s xs = foo : chunks s bar
              where (foo, bar) = (splitAt s xs)

