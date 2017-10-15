{-# LANGUAGE OverloadedStrings   #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
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
               test


runHTTPRequests :: [Request]
                -> IO [Either HttpException (Response BL.ByteString)]
runHTTPRequests reqs = do
    mgr <- newManager defaultManagerSettings
    mapM (try . flip httpLbs mgr) reqs

test :: IO ()
test = do
  (dbcfg, coincfg) <- defaultBlocksqldConfig
  rs <- runReaderT (mapM (fromRpcToRequest . getblockhash) [1..3]) coincfg
  resp <- runHTTPRequests rs
  --let resp2 = fmap f resp
  print resp

testRequests :: IO [Request]
testRequests = do
  (dbcfg, coincfg) <- defaultBlocksqldConfig
  runReaderT (mapM (fromRpcToRequest . getblockhash) [1..3]) coincfg

defaultBlocksqldConfig :: IO (DBConfig, CoinConf)
defaultBlocksqldConfig = parseConfig "config.txt"

