{-# LANGUAGE OverloadedStrings   #-}
module Main where

import Control.Error.Util
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

import Database.Persist.Sql

import Text.Pretty.Simple (pPrint)

import Network.HTTP.Client
import Network.HTTP.Types

import Blocksqld.Types
import Blocksqld.Database
import Blocksqld.Commands


main :: IO ()
main = do
  (dbcfg, coincfg) <- defaultBlocksqldConfig
  runReaderT startDB dbcfg
  forM_ [1..] $ \i -> do
      threadDelay 2000000
      pPrint =<< runCommand (getblockWithHeight i) coincfg

defaultBlocksqldConfig :: IO (DBConfig, CoinConf)
defaultBlocksqldConfig = parseConfig "config.txt"



