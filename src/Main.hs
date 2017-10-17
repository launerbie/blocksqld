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
  forever $ do threadDelay 2000000
               test coincfg

test :: CoinConf ->IO ()
test cfg = do
  mBlocks <- runCommand (mapM getblockWithHeight [1..5]) cfg
  pPrint mBlocks

defaultBlocksqldConfig :: IO (DBConfig, CoinConf)
defaultBlocksqldConfig = parseConfig "config.txt"



