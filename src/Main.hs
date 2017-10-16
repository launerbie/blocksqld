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
               test

runHTTPRequest :: Request
               -> Manager
               -> IO (Either HttpException (Response BL.ByteString))
runHTTPRequest req mgr = (try . flip httpLbs mgr) req

runHTTPRequests :: [Request]
                -> IO [Either HttpException (Response BL.ByteString)]
runHTTPRequests reqs = do
    mgr <- newManager defaultManagerSettings
    mapM (flip runHTTPRequest mgr) reqs

toRpcResponse :: Either HttpException (Response BL.ByteString)
              -> Maybe RpcResponse
toRpcResponse e = case e of
                     Left _     -> Nothing
                     Right resp -> decode (responseBody resp)

decodeResponseWith :: FromJSON a
                   => BL.ByteString
                   -> (BL.ByteString -> Maybe a)
                   -> Maybe a
decodeResponseWith json f = f json

test :: IO ()
test = do
  (dbcfg, coincfg) <- defaultBlocksqldConfig
  rs <- runReaderT (mapM (jsonRpcToHTTPRequest . getblockhash) [1..3]) coincfg
  resp <- runHTTPRequests rs
  --let resp2 = fmap f resp
  pPrint resp

testRequests :: IO [Request]
testRequests = do
  (dbcfg, coincfg) <- defaultBlocksqldConfig
  runReaderT (mapM (jsonRpcToHTTPRequest . getblockhash) [1..3]) coincfg

defaultBlocksqldConfig :: IO (DBConfig, CoinConf)
defaultBlocksqldConfig = parseConfig "config.txt"



